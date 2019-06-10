package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike._
import structures._
import structures.immutable.HyperGraphManyWithOrderToOne
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
    logger.trace(s"Running rewrite rule $this")
    val compactGraph = state.graph

    // Fill conditions - maybe subgraph matching instead of current temple

    val premiseReferencesMaps = compactGraph.findSubgraphVersioned[Int](subGraphPremise, lastVersion)

    val nextHyperId: () => HyperTermId = {
      val creator = Stream.from(compactGraph.nodes.map(_.id).reduceLeftOption(_ max _).getOrElse(0) + 1).map(HyperTermId).toIterator
      () => creator.next
    }

    val existentialsMax = {
      val temp = state.graph.edgeTypes.filter(_.identifier.literal.toString.startsWith("existential")).map(_.identifier.literal.toString.drop("existential".length).toInt)
      if (temp.isEmpty) -1 else temp.max
    }

    val newEdges = premiseReferencesMaps.flatMap(m => {
      val meta = metaCreator(m._1, m._2).merge(metadataCreator(HyperGraphManyWithOrderToOneLike.mergeMap[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](premise, m)))
      val merged = HyperGraphManyWithOrderToOneLike.mergeMap[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](subGraphConclusion(existentialsMax, meta), m)
      if (compactGraph.findSubgraph[Int](merged).nonEmpty) Seq.empty
      else HyperGraphManyWithOrderToOneLike.fillWithNewHoles[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](merged, nextHyperId).map(e =>
        e.copy(metadata = e.metadata.merge(meta)))
    })
    // Should crash if we still have holes as its a bug
    val graph = compactGraph :+ newEdges
    if (graph.size > compactGraph.size) {
      logger.debug(s"Used RewriteRule $this")
    }

    (new RewriteSearchState(graph), graph.version)
  }

  /* --- Privates --- */

  val metadataCreator: RewriteRule.SubHyperGraphPattern => RewriteRuleMetadata = RewriteRuleMetadata.curried(this)

  private val subGraphPremise: SubHyperGraphPattern = premise

  // Existential cannot be a function
  private val destHoles = conclusion.edges.flatMap(_.sources).filter(_.isInstanceOf[Hole[HyperTermId, Int]]).diff(conclusion.edges.map(_.target))
  private val condHoles = premise.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  private def subGraphConclusion(maxExist: Int, metadata: Metadata): SubHyperGraphPattern = {
    // TODO: change to Uid from Programs instead of global
    val existentialEdges = existentialHoles.zipWithIndex.map(((existentialHole: Template.TemplateTerm[HyperTermId], index: Int) => {
      HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](existentialHole,
        Explicit(HyperTermIdentifier(Identifier(s"existential${maxExist + index + 1}", namespace = Some(new Namespace {})))), Seq.empty, metadata)
    }).tupled)
    conclusion.addEdges(existentialEdges)
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = HyperGraphManyWithOrderToOne[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: RewriteRule, originalEdges: RewriteRule.SubHyperGraphPattern) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin, $originalEdges)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = HyperGraphManyWithOrderToOne(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata)).toSeq: _*
  )

  /* --- Privates --- */

  private type SubHyperGraphPattern = HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
