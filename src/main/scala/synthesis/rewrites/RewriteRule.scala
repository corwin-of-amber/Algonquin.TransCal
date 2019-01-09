package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.HyperGraphManyWithOrderToOneLike._
import structures.immutable.HyperGraphManyWithOrderToOne
import syntax.AstSugar.Uid
import syntax.Identifier
import synthesis.rewrites.RewriteRule._
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}

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
    logger.trace("Creating a new state")
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsReferencesMaps = compactGraph.findSubgraph[Int, SubHyperGraphPattern](subGraphConditions)

    def extractNewEdges(m: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier])): Set[HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]] = {
      m._1.foldLeft(m._2.foldLeft(subGraphDestination)((graph, kv) => {
        // From each map create new edges from the destination graph
        graph.mergeEdgeTypes(Explicit(kv._2), Hole(kv._1))
      }))((graph, kv) => {
        // From each map create new edges from the destination graph
        graph.mergeNodes(Explicit(kv._2), Hole(kv._1))
      }).edges
    }

    val lastId = compactGraph.nodes.map(_.id).max
    def extract(i: Item[HyperTermId, Int]): HyperTermId = i match {
      case Explicit(v) => v
      case Hole(v) => HyperTermId(v + lastId)
    }

    // Should crash if we still have holes as its a bug
    val graph = compactGraph :+ conditionsReferencesMaps.flatMap(m => {
      val meta = metaCreator(m._1, m._2).merge(metadata)
      extractNewEdges(m).map(e =>
        HyperEdge(extract(e.target),
          e.edgeType match { case Explicit(v) => v },
          e.sources.map(extract),
          e.metadata.merge(meta))
      )
    })

    new RewriteSearchState(graph)
  }

  /* --- Privates --- */

  val metadata: RewriteRuleMetadata = RewriteRuleMetadata(this)

  /** This function should work only after "id" rule.
    *
    * @param graph
    * @return
    */
  private def compact(graph: HyperGraph): HyperGraph = {
    logger.trace("Compacting graph")
    graph
  }

  private def termToHyperItem[T <: HyperTerm](templateTerm: TemplateTerm[T]): Item[T, Int] = templateTerm match {
    case ReferenceTerm(i) => Hole(i)
    case ExplicitTerm(term) => Explicit(term)
  }

  private val subGraphConditions: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = conditions.edges.map(e =>
      HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](
        termToHyperItem(e.target), termToHyperItem(e.edgeType), e.sources.map(termToHyperItem), EmptyMetadata)
    )
    HyperGraphManyWithOrderToOne(edges)
  }

  // Existential cannot be a function
  private val destHoles = destination.nodes.map(termToHyperItem).filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val condHoles = conditions.nodes.map(termToHyperItem).filter(_.isInstanceOf[Hole[HyperTermId, Int]])
  private val existentialHoles = destHoles.diff(condHoles)

  private def subGraphDestination: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = destination.edges.map(e =>
      HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](
        termToHyperItem(e.target), termToHyperItem(e.edgeType), e.sources.map(termToHyperItem), EmptyMetadata)
    )

    // TODO: change to Uid from Programs instead of global
    // TODO: prevent existentials from being recreated.
    val existentialEdges = existentialHoles.map(HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](
      _, Explicit(HyperTermIdentifier(new Identifier("ex?", "variable", new Uid))), Seq.empty, metadata))
    HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](edges ++ existentialEdges)
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = HyperGraphManyWithOrderToOne[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]

  case class RewriteRuleMetadata(origin: RewriteRule) extends Metadata {
    override def toStr: String = s"RewriteRuleMetadata($origin)"
  }

  object CategoryMetadata extends Enumeration with Metadata {
    val Basic, Associative, Goal, Definition, Existential = Value

    override protected def toStr: String = this.getClass.getName
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = HyperGraphManyWithOrderToOne(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters, EmptyMetadata))
  )

  /* --- Privates --- */

  private type SubHyperGraphPattern = HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
