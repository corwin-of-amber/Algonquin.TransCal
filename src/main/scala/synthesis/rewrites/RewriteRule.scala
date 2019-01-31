package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike._
import structures._
import structures.immutable.HyperGraphManyWithOrderToOne
import syntax.AstSugar.Uid
import syntax.Identifier
import synthesis.rewrites.RewriteRule._
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.rewrites._

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(conditions: HyperPattern,
                  destination: HyperPattern,
                  metaCreator: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata)
  extends Operator[RewriteSearchState] with LazyLogging {

  /* --- Operator Impl. --- */
  override def toString: String = s"RewriteRule($conditions, $destination)"

  // Add metadata creator
  override def apply(state: RewriteSearchState): RewriteSearchState = {
    logger.trace(s"Running rewrite rule $this")
    val compactGraph = state.graph

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsReferencesMaps = compactGraph.findSubgraph(subGraphConditions)

    val nextHyperId: () => HyperTermId = {
      val creator = Stream.from(compactGraph.nodes.map(_.id).reduceLeftOption(_ max _).getOrElse(0) + 1).map(HyperTermId).toIterator
      () => creator.next
    }

    val newEdges = conditionsReferencesMaps.flatMap(m => {
      val meta = metaCreator(m._1, m._2).merge(metadata)
      val merged = HyperGraphManyWithOrderToOneLike.mergeMap[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](subGraphDestination, m)
      if (compactGraph.findSubgraph(merged).nonEmpty) Seq.empty
      else HyperGraphManyWithOrderToOneLike.fillWithNewHoles[HyperTermId, HyperTermIdentifier, Int, SubHyperGraphPattern](merged, nextHyperId).map(e =>
        e.copy(metadata=e.metadata.merge(meta)))
    })
    // Should crash if we still have holes as its a bug
    val graph = compactGraph :+ newEdges

    new RewriteSearchState(graph)
  }

  /* --- Privates --- */

  val metadata: RewriteRuleMetadata = RewriteRuleMetadata(this)

  private val subGraphConditions: SubHyperGraphPattern = conditions

  // Existential cannot be a function
  private val destHoles = destination.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val condHoles = conditions.nodes.filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  private def subGraphDestination: SubHyperGraphPattern = {
    // TODO: change to Uid from Programs instead of global
    // TODO: prevent existentials from being recreated.
//    val existentialEdges = existentialHoles.map(HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](
//      _, Explicit(HyperTermIdentifier(new Identifier("ex?", "variable", new Uid))), Seq.empty, metadata))
//    val existentialEdges = existentialHoles.map(patternEdgeCreator(_, new Identifier("ex?", "variable", new Uid), Seq.empty))
//    destination.addEdges(existentialEdges)
    destination
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
