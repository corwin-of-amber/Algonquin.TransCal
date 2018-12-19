package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike._
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.rewrites.RewriteRule.{Category, HyperPattern, SubHyperEdgePattern, SubHyperGraphPattern}
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier}

/** Rewrites a program to a new program.
  *
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(conditions: HyperPattern, destination: HyperPattern, ruleType: Category.Value) extends Operator[RewriteSearchState] with LazyLogging {


  /* --- Operator Impl. --- */

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    logger.trace("Creating a new state")
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsReferencesMaps = compactGraph.findSubgraph[Int, SubHyperGraphPattern](subGraphConditions)

    def extractNewEdges(m: Map[Int, Either[HyperTermId, HyperTermIdentifier]]): Set[HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]] = {
      m.foldLeft(subGraphDestination)((graph, kv) => {
        // From each map create new edges from the destination graph
        kv._2 match {
          case Right(k) => graph.mergeEdgeTypes(Explicit(k), Hole(kv._1))
          case Left(k) => graph.mergeNodes(Explicit(k), Hole(kv._1))
        }
      }).edges
    }

    def extract[T](i: Item[T, Int]): T = i match {
      case Explicit(v) => v
    }

    // Should crash if we still have holes as its a bug
    val graph = compactGraph.addEdges(conditionsReferencesMaps.flatMap(m => extractNewEdges(m)).map(e =>
      HyperEdge(extract[HyperTermId](e.target), extract[HyperTermIdentifier](e.edgeType), e.sources.map(extract[HyperTermId]))
    ))

    new RewriteSearchState(graph)
  }


  /* --- Privates --- */

  /** This function should work only after "id" rule.
    *
    * @param graph
    * @return
    */
  private def compact(graph: HyperGraph): HyperGraph = {
    logger.trace("Compacting graph")
    graph
  }


  private def termToHyperItem(templateTerm: TemplateTerm): Item[HyperTermId, Int] = templateTerm match {
    case ReferenceTerm(i) => Hole(i)
    case ExplicitTerm(term) => Explicit(term.asInstanceOf[HyperTermId])
  }


  private def termToHyperIdentifierItem(templateTerm: TemplateTerm): Item[HyperTermIdentifier, Int] = templateTerm match {
    case ReferenceTerm(i) => Hole(i)
    case ExplicitTerm(term) => Explicit(term.asInstanceOf[HyperTermIdentifier])
  }

  private val subGraphConditions: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = conditions.edges.map(e =>
      HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](termToHyperItem(e.target), termToHyperIdentifierItem(e.edgeType), e.sources.map(termToHyperItem))
    )
    HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](edges)
  }

  private val subGraphDestination: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = destination.edges.map(e =>
      HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](termToHyperItem(e.target), termToHyperIdentifierItem(e.edgeType), e.sources.map(termToHyperItem))
    )
    HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]](edges)
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = HyperGraphManyWithOrderToOne[TemplateTerm, TemplateTerm]

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }

  def createHyperPatternFromTemplates(templates: Set[Template]): HyperPattern = HyperGraphManyWithOrderToOne.apply(
    templates.map(pattern => HyperEdge(pattern.target, pattern.function, pattern.parameters))
  )

  /* --- Privates --- */

  private type SubHyperGraphPattern = HyperGraphManyWithOrderToOne[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]]
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]
}
