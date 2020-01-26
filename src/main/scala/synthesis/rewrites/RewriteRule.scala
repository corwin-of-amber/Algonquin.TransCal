package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import structures.mutable.HyperGraph
import synthesis.rewrites.RewriteRule._
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.{StepOperator, VersionedOperator}
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}
import transcallang.{Identifier, Namespace}

import scala.annotation.tailrec

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(val premise: HyperPattern,
                  val conclusion: HyperPattern,
                  val metaCreator: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata,
                  val termString: String = null) extends VersionedOperator[RewriteSearchState]
  with StepOperator[Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]], RewriteSearchState] with LazyLogging {
  /* --- Operator Impl. --- */
  override def toString: String = s"RewriteRule(${'"'}$termString${'"'}, $premise, $conclusion)"

  override def hashCode(): Int = toString.hashCode

  require(conclusion.nodes.forall({
    case Ignored() => false
    case _ => true
  }))

  private val mutablePremise = mutable.HyperGraph(premise.toSeq: _*)
  private val conclusionExpansionNeeded = conclusion.nodes.exists(_.isInstanceOf[Repetition[HyperTermId, Int]])
  private val conclusionMutable = mutable.CompactHyperGraph(conclusion.toSeq: _*).asInstanceOf[RewriteRule.MutableHyperPattern]

  // Add metadata creator
  override def apply(state: RewriteSearchState): (RewriteSearchState) = {
    logger.trace(s"Running rewrite rule $this")

    val halfFilledPatterns = fillConclusions(state, false)
    if (halfFilledPatterns.nonEmpty) {
      logger.debug(s"Used RewriteRule $this ")
    }
    state.graph ++= getConclusionsByState(state, halfFilledPatterns)
    (state)
  }

  /* --- Privates --- */

  private def getConclusionsByState(state: RewriteSearchState,
                                    halfFilledPatterns: Set[(mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]], Metadata)]) = {
    val nextHyperId: () => HyperTermId = {
      val creator = Stream.from(if (state.graph.isEmpty) 0 else state.graph.nodes.map(_.id).max + 1).map(HyperTermId).iterator
      () => creator.next
    }

    halfFilledPatterns.flatMap({
      case (p, meta) =>
        mutable.HyperGraph.fillWithNewHoles(p, nextHyperId).map(e => e.copy(metadata = e.metadata.merge(meta)))
    })
  }

  private def fillConclusions(state: RewriteSearchState, versioned: Boolean) = {
    // Fill conditions - maybe subgraph matching instead of current temple
    val premiseReferencesMaps =
      if (versioned) state.graph.findSubgraphVersioned[Int](subGraphPremise)
      else state.graph.findSubgraph[Int](subGraphPremise)

    val edgeToTarget = collection.mutable.HashMap.empty[(HyperTermIdentifier, Seq[HyperTermId]), HyperTermId]
    @tailrec
    def fillKnownTargets(halfFilled: mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]): mutable.HyperGraph[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] = {
      val toFill = halfFilled.edges
        .collect({
          case HyperEdge(t: ReferenceTerm[HyperTermId], et: ExplicitTerm[HyperTermIdentifier], s: Seq[Item[HyperTermId, Int]], _) if s.forall(_.isInstanceOf[ExplicitTerm[HyperTermId]]) =>
            (t.id, et.value, s.map(_.asInstanceOf[ExplicitTerm[HyperTermId]].value))
        })
      val fillValues = toFill.flatMap({case (t, et, s) =>
        edgeToTarget.get((et, s)).map((t, _))
        if (edgeToTarget.contains((et, s))) Some((t, edgeToTarget((et, s))))
        else {
          state.graph.findRegex[Int](HyperEdge(ReferenceTerm(t), ExplicitTerm(et), s.map(ExplicitTerm(_)), EmptyMetadata))
            .headOption.map(tup => {
            edgeToTarget((et, s)) = tup._2(t)
            (t, tup._2(t))
          })
        }
      }).toMap
      if (fillValues.isEmpty) halfFilled
      else fillKnownTargets(mutable.HyperGraph.mergeMap(halfFilled, (fillValues, Map.empty)))
    }

    val halfFilledPatterns = premiseReferencesMaps.flatMap(m => {
      // TODO: undo this. just want better runtime then listing the graph a million times.
//      val meta = metaCreator(m._1, m._2).merge(metadataCreator(mutable.HyperGraph.mergeMap(mutablePremise.clone(), m)))
      val meta = metaCreator(m._1, m._2)
      val merged = mutable.HyperGraph.mergeMap(subGraphConclusion(state.graph, meta), m)
      val furtherFilling = fillKnownTargets(merged)
      if (state.graph.findSubgraph[Int](furtherFilling).nonEmpty) None
      else Some((furtherFilling, meta))
    })
    halfFilledPatterns
  }

  val metadataCreator: RewriteRule.HyperPattern => RewriteRuleMetadata = RewriteRuleMetadata.curried(this)

  private val subGraphPremise: HyperPattern = premise

  // Existential cannot be a function
  private val destHoles = conclusion.edges.flatMap(_.sources).filter(_.isInstanceOf[Hole[HyperTermId, Int]]).diff(conclusion.targets)
  private val condHoles = premise.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  def isExistential: Boolean = existentialHoles.nonEmpty

  private def subGraphConclusion(graph: RewriteSearchState.HyperGraph, metadata: Metadata): MutableHyperPattern = {
    if (existentialHoles.nonEmpty) {
      val existentialsMax = {
        val temp = graph.edgeTypes.filter(_.identifier.literal.toString.startsWith("existential")).map(_.identifier.literal.toString.drop("existential".length).toInt)
        if (temp.isEmpty) -1 else temp.max
      }

      // TODO: change to Uid from Programs instead of global
      val existentialEdges = existentialHoles.zipWithIndex.map({ case (existentialHole: Template.TemplateTerm[HyperTermId], index: Int) =>
        HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](existentialHole,
          Explicit(HyperTermIdentifier(Identifier(s"existential${existentialsMax + index + 1}", namespace = Some(new Namespace {})))), Seq.empty, metadata)
      })
      conclusionMutable.clone().++=(existentialEdges)
    }
    else conclusionMutable.clone()
  }

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param state state on which to run operator
    * @return (new state after update, next relevant version)
    */
  override def applyVersioned(state: RewriteSearchState): RewriteSearchState = {
    logger.trace(s"Running rewrite rule $this")

    val halfFilledPatterns = fillConclusions(state, true)
    if (halfFilledPatterns.nonEmpty) {
      logger.debug(s"Used RewriteRule $this ")
    }
    state.graph ++= getConclusionsByState(state, halfFilledPatterns)
    (state)
  }

  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param state     current state from which to do the initial calculations and create an operator
    * @param versioned if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(state: RewriteSearchState, versioned: Boolean): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    val graphsAndMetas = fillConclusions(state, versioned)
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

    val res = graphsAndMetas.filter(_._1.nonEmpty).flatMap({ case (g, m) =>
      val moved = moveHoles(g).map(e => e.copy(metadata = e.metadata.merge(m)))
      maxHole = Math.max(maxHole, moved.map(e => (Seq(toId(e.target), toId(e.edgeType)) ++ e.sources.map(toId[HyperTermId])).max).max)
      moved
    })
    if (res.nonEmpty) logger.debug(s"Stepped with RewriteRule $this")
    res
  }
}

object RewriteRule {

  /* --- Public --- */
  type HyperPattern = immutable.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int]
  private type MutableHyperPattern = mutable.HyperGraph[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: RewriteRule, originalEdges: RewriteRule.HyperPattern) extends Metadata {
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
            fillPatterns(hyperGraph, rest.map(generic.HyperGraph.mergeMap(_, maps)))
              .map(a => fullPattern +: a)
        }
    }
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = immutable.HyperGraph(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata)).toSeq: _*
  )

  /* --- Privates --- */
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
