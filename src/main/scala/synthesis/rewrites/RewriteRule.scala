package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import structures.immutable.HyperGraph
import synthesis.rewrites.RewriteRule._
import synthesis.rewrites.Template.TemplateTerm
import synthesis.search.VersionedOperator
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.{Identifier, Namespace}

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(val premise: HyperPattern,
                  val conclusion: HyperPattern,
                  val metaCreator: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata)
  extends VersionedOperator[RewriteSearchState] with LazyLogging {

  /* --- Operator Impl. --- */
  override def toString: String = s"RewriteRule($premise, $conclusion)"

  override def hashCode(): Int = toString.hashCode

  // Add metadata creator
  override def apply(state: RewriteSearchState, lastVersion: Long): (RewriteSearchState, Long) = {
    logger.trace(s"Running rewrite rule $this ")
    val compactGraph = state.graph

    // Fill conditions - maybe subgraph matching instead of current temple

    val premiseReferencesMaps = compactGraph.findSubgraphVersioned[Int](subGraphPremise, lastVersion)

    if (premiseReferencesMaps.nonEmpty) {
      val nextHyperId: () => HyperTermId = {
        val creator = Stream.from(if (compactGraph.isEmpty) 0 else compactGraph.nodes.map(_.id).max + 1).map(HyperTermId).iterator
        () => creator.next
      }

      val newEdges = premiseReferencesMaps.flatMap(m => {
        val meta = metaCreator(m._1, m._2).merge(metadataCreator(generic.HyperGraph.mergeMap(premise, m)))
        val merged = generic.HyperGraph.mergeMap(subGraphConclusion(state.graph, meta), m)
        if (compactGraph.findSubgraph[Int](merged).nonEmpty) Seq.empty
        else generic.HyperGraph.fillWithNewHoles(merged, nextHyperId).map(e =>
          e.copy(metadata = e.metadata.merge(meta)))
      })

      compactGraph ++= newEdges
      if (newEdges.nonEmpty) {
        logger.debug(s"Used RewriteRule $this ")
      }
    }

    (new RewriteSearchState(compactGraph), compactGraph.version)
  }

  /* --- Privates --- */

  val metadataCreator: RewriteRule.HyperPattern => RewriteRuleMetadata = RewriteRuleMetadata.curried(this)

  private val subGraphPremise: HyperPattern = premise

  // Existential cannot be a function
  private val destHoles = conclusion.edges.flatMap(_.sources).filter(_.isInstanceOf[Hole[HyperTermId, Int]]).diff(conclusion.targets)
  private val condHoles = premise.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  private def subGraphConclusion(graph: RewriteSearchState.HyperGraph, metadata: Metadata): HyperPattern = {
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
      conclusion.++(existentialEdges)
    }
    else conclusion
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = immutable.HyperGraph[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: RewriteRule, originalEdges: RewriteRule.HyperPattern) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin, $originalEdges)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def fillPatterns(hyperGraph: HyperGraph[HyperTermId, HyperTermIdentifier], patterns: Seq[HyperPattern]): Iterator[Seq[Set[HyperEdge[HyperTermId, HyperTermIdentifier]]]] = {
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
