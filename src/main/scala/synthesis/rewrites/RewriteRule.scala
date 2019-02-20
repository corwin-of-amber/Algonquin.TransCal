package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike._
import structures._
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.rewrites.RewriteRule._
import synthesis.rewrites.Template.TemplateTerm
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier}

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(premise: HyperPattern,
                  conclusion: HyperPattern,
                  metaCreator: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata)
  extends Operator[RewriteSearchState] with LazyLogging {

  /* --- Operator Impl. --- */
  override def toString: String = s"RewriteRule($premise, $conclusion)"

  // Add metadata creator
  override def apply(state: RewriteSearchState): RewriteSearchState = {
    logger.trace(s"Running rewrite rule $this")
    val compactGraph = state.graph

    // Fill conditions - maybe subgraph matching instead of current temple

    val premiseReferencesMaps = compactGraph.findSubgraph[Int](subGraphPremise)

    val nextHyperId: () => HyperTermId = {
      val creator = Stream.from(compactGraph.nodes.map(_.id).reduceLeftOption(_ max _).getOrElse(0) + 1).map(HyperTermId).toIterator
      () => creator.next
    }

    val newEdges = premiseReferencesMaps.flatMap(m => {
      val meta = metaCreator(m._1, m._2).merge(metadata)
      val merged = HyperGraphManyWithOrderToOneLike.mergeMap[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](subGraphConclusion, m)
      if (compactGraph.findSubgraph[Int](merged).nonEmpty) Seq.empty
      else HyperGraphManyWithOrderToOneLike.fillWithNewHoles[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](merged, nextHyperId).map(e =>
        e.copy(metadata=e.metadata.merge(meta)))
    })
    // Should crash if we still have holes as its a bug
    val graph = compactGraph :+ newEdges
    if (graph.size > compactGraph.size) {
      logger.debug(s"Used RewriteRule $this")
    }

    new RewriteSearchState(graph)
  }

  /* --- Privates --- */

  val metadata: RewriteRuleMetadata = RewriteRuleMetadata(this)

  private val subGraphPremise: SubHyperGraphPattern = premise

  // Existential cannot be a function
  private val destHoles = conclusion.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val condHoles = premise.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  private def subGraphConclusion: SubHyperGraphPattern = {
    // TODO: change to Uid from Programs instead of global
    // TODO: prevent existentials from being recreated.
//    val existentialEdges = existentialHoles.map(HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](
//      _, Explicit(HyperTermIdentifier(new Identifier("ex?", "variable", new Uid))), Seq.empty, metadata))
//    val existentialEdges = existentialHoles.map(patternEdgeCreator(_, new Identifier("ex?", "variable", new Uid), Seq.empty))
//    destination.addEdges(existentialEdges)
    conclusion
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = HyperGraphManyWithOrderToOne[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]
  type HyperPatternEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: RewriteRule) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = HyperGraphManyWithOrderToOne(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata)).toSeq:_*
  )

  /* --- Privates --- */

  private type SubHyperGraphPattern = HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
