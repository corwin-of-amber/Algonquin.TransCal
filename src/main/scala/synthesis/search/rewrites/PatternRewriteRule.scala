package synthesis.search.rewrites

import com.typesafe.scalalogging.LazyLogging
import report.Stats
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import structures.generic.HyperGraph
import structures.generic.HyperGraph.Match
import synthesis.search.rewrites.PatternRewriteRule.{HyperPattern, MutableHyperPattern, RewriteRuleMetadata}
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm, RepetitionTerm, TemplateTerm}
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}
import transcallang.{Identifier, Namespace}

import scala.annotation.tailrec

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class PatternRewriteRule(val premise: HyperPattern,
                         val conclusion: HyperPattern,
                         val termString: String = null,
                         val postProcessors: Seq[(HyperGraph.Match[HyperTermId, HyperTermIdentifier, Int], MutableHyperPattern) => MutableHyperPattern] = Seq.empty) extends RewriteRule with LazyLogging {
  /* --- Operator Impl. --- */
  override def toString: String = s"RewriteRule(${'"'}$termString${'"'}, $premise, $conclusion)"

  override def hashCode(): Int = toString.hashCode

  require(!conclusion.nodes.contains(Ignored()))

  private val mutablePremise = mutable.HyperGraph(premise.toSeq: _*)
  private val conclusionExpansionNeeded = conclusion.nodes.exists(_.isInstanceOf[Repetition[HyperTermId, Int]])
  private val conclusionMutable = mutable.CompactHyperGraph(conclusion.toSeq: _*).asInstanceOf[PatternRewriteRule.MutableHyperPattern]

  def withTermString(termString: String) = new PatternRewriteRule(premise, conclusion, termString)

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param state state on which to run operator
    * @return (new state after update, next relevant version)
    */
  override def apply(state: RewriteRule.HyperGraph): Unit = innerApply(state, false)
  override def applyVersioned(state: RewriteRule.HyperGraph): Unit = innerApply(state, true)

  // Add metadata creator
  def innerApply(graph: RewriteRule.HyperGraph, versioned: Boolean): Unit = {
      logger.trace(s"Running rewrite rule $this")

      val halfFilledPatterns = fillConclusions(graph, versioned)
      if (halfFilledPatterns.nonEmpty) {
        logger.debug(s"Used RewriteRule $this ")
      }
      graph ++= getConclusionsByState(graph, halfFilledPatterns)
    }

  /* --- Privates --- */

  private def getConclusionsByState(graph: RewriteRule.HyperGraph,
                                    halfFilledPatterns: Set[mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]]) = {
    val nextHyperId: () => HyperTermId = {
      val creator = Stream.from(if (graph.isEmpty) 0 else graph.nodes.map(_.id).max + 1).map(HyperTermId).iterator
      () => creator.next
    }

    halfFilledPatterns.flatMap({ case p =>
        mutable.HyperGraph.fillWithNewHoles(p, nextHyperId)
    })
  }

  private def fillConclusions(graph: RewriteRule.HyperGraph, versioned: Boolean) = {
    // Fill conditions - maybe subgraph matching instead of current temple
    val premiseMatches =
      if (versioned) graph.findSubgraphVersioned[Int](subGraphPremise)
      else graph.findSubgraph[Int](subGraphPremise)

    val edgeToTarget = collection.mutable.HashMap.empty[(HyperTermIdentifier, Seq[HyperTermId]), HyperTermId]

    @tailrec
    def fillKnownTargets(halfFilled: mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]])
    : mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
      val toFill = halfFilled.edges
        .collect({
          case HyperEdge(t: ReferenceTerm[HyperTermId], et: ExplicitTerm[HyperTermIdentifier], s: Seq[Item[HyperTermId, Int]], _) if s.forall(_.isInstanceOf[ExplicitTerm[HyperTermId]]) =>
            (t.id, et.value, s.map(_.asInstanceOf[ExplicitTerm[HyperTermId]].value))
        })
      val fillValues = toFill.flatMap({ case (t, et, s) =>
        edgeToTarget.get((et, s)).map((t, _))
        if (edgeToTarget.contains((et, s))) Some((t, edgeToTarget((et, s))))
        else {
          graph.findRegex[Int](HyperEdge(ReferenceTerm(t), ExplicitTerm(et), s.map(ExplicitTerm(_)), EmptyMetadata))
            .headOption.map(matched => {
            edgeToTarget((et, s)) = matched.nodeMap(t)
            (t, matched.nodeMap(t))
          })
        }
      }).toMap
      if (fillValues.isEmpty) halfFilled
      else fillKnownTargets(mutable.HyperGraph.mergeMatch(halfFilled, Match(Set.empty, fillValues, Map.empty)))
    }

    val halfFilledPatterns = premiseMatches.flatMap(m => {
      // TODO: undo this. just want better runtime then listing the graph a million times.
      //      val meta = metaCreator(m._1, m._2).merge(metadataCreator(mutable.HyperGraph.mergeMap(mutablePremise.clone(), m)))
      val meta = UnionMetadata(creators.map(_(m)).toSet)
      val merged = mutable.HyperGraph.mergeMatch(subGraphConclusion(graph, meta, m), m)
      val furtherFilling = fillKnownTargets(merged)
      if (graph.findSubgraph[Int](furtherFilling).nonEmpty) None
      else Some(furtherFilling)
    })
    halfFilledPatterns
  }

  val metadataCreator: PatternRewriteRule.HyperPattern => RewriteRuleMetadata = RewriteRuleMetadata.curried(this)

  private val subGraphPremise: HyperPattern = premise

  // Existential cannot be a function
  private val destHoles = conclusion.edges.flatMap(_.sources).filter(_.isInstanceOf[Hole[HyperTermId, Int]]).diff(conclusion.targets)
  private val condHoles = premise.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)
  private val repetitionEdges = conclusion.edges.filter(_.sources.exists(_.isInstanceOf[RepetitionTerm[HyperTermId]]))

  def isExistential: Boolean = existentialHoles.nonEmpty

  private def subGraphConclusion(graph: RewriteRule.HyperGraph, metadata: Metadata, matched: HyperGraph.Match[HyperTermId, HyperTermIdentifier, Int]): MutableHyperPattern = {
    val withExist = conclusionMutable.clone()
    if (existentialHoles.nonEmpty) {
      val existentialsMax = {
        val temp = graph.edgeTypes.filter(_.identifier.literal.startsWith("existential")).map(_.identifier.literal.drop("existential".length).toInt)
        if (temp.isEmpty) -1 else temp.max
      }

      // TODO: change to Uid from Programs instead of global
      val existentialEdges = existentialHoles.zipWithIndex.map({ case (existentialHole: Template.TemplateTerm[HyperTermId], index: Int) =>
        HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](existentialHole,
          Explicit(HyperTermIdentifier(Identifier(s"existential${existentialsMax + index + 1}", namespace = Some(new Namespace {})))), Seq.empty, metadata)
      })
      withExist.++=(existentialEdges)
    }
    for (re <- repetitionEdges) {
      val newEdge = re.copy(sources = re.sources.flatMap({
        case Repetition(min, max, rep) =>
          // This is ok because:
          //  a. for each min to max we will have a different match (different m)
          //  b. max will be reached during takewhile because the original findsubgraph is finite
          val start = rep.take(min)
          assert(start.forall(item => (!item.isInstanceOf[RepetitionTerm[HyperTermId]]) && !item.isInstanceOf[Ignored[HyperTermId, Int]]))
          start.toSeq ++ rep.drop(min).takeWhile({
            case ExplicitTerm(_) => true
            case ReferenceTerm(id) => matched.nodeMap.contains(id)
            case Repetition(_, _, _) => throw new IllegalArgumentException("Can't have nested repetitions")
            case Ignored() => throw new IllegalArgumentException("Can't have Ignored in conclusions")
          })
        case i => Seq(i)
      }))
      withExist.-=(re).+=(newEdge)
    }
    withExist.foreach(e => withExist.updateMetadata(e, metadata))
    postProcessors.foldLeft(withExist)({case (g, pp) => pp(matched, g)})
  }

  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param graph     current graph from which to do the initial calculations and create an operator
    * @param versioned if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(graph: RewriteRule.HyperGraph, versioned: Boolean): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    val graphs = fillConclusions(graph, versioned)
    var maxHole = 0

    def moveHoles(graph: mutable.HyperGraph[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
      def ifHoleMoveElseCopy[T <: HyperTerm](t: TemplateTerm[T]): TemplateTerm[T] = t match {
        case r: ReferenceTerm[T] => ReferenceTerm[T](r.id + maxHole)
        case a: TemplateTerm[T] => a
      }

      graph.edges.map(e => e.copy(ifHoleMoveElseCopy(e.target), ifHoleMoveElseCopy(e.edgeType), e.sources.map(ifHoleMoveElseCopy)))
    }

    def toId[T <: HyperTerm](t: TemplateTerm[T]): Int = t match {
      case t: ReferenceTerm[T] => t.id
      case _ => 0
    }

    val res = graphs.filter(_.nonEmpty).flatMap({ case g =>
      val moved = moveHoles(g)
      maxHole = Math.max(maxHole, moved.map(e => (Seq(toId(e.target), toId(e.edgeType)) ++ e.sources.map(toId[HyperTermId])).max).max)
      moved
    })
    if (res.nonEmpty) {
      logger.debug(s"Stepped with RewriteRule $this")
      Stats.instance.ruleUsage.inc(this, res.size)
    }
    res
  }
}

object PatternRewriteRule {

  /* --- Public --- */
  type HyperPattern = generic.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int]
  type MutableHyperPattern = mutable.HyperGraph[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: PatternRewriteRule, originalEdges: PatternRewriteRule.HyperPattern) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin, $originalEdges)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def fillPatterns(hyperGraph: generic.HyperGraph[HyperTermId, HyperTermIdentifier], patterns: Seq[HyperPattern]): Iterator[Seq[Set[HyperEdge[HyperTermId, HyperTermIdentifier]]]] = {
    patterns match {
      case Nil => Iterator(Seq.empty)
      case pattern :: rest =>
        hyperGraph.findSubgraph[Int](pattern).iterator.flatMap {
          maps =>
            val fullPattern = generic.HyperGraph.fillPattern(pattern, maps, () => throw new RuntimeException("unknown reason"))
            fillPatterns(hyperGraph, rest.map(generic.HyperGraph.mergeMatch(_, maps)))
              .map(a => fullPattern +: a)
        }
    }
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = generic.HyperGraph(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata)).toSeq: _*
  )

  /* --- Privates --- */
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
