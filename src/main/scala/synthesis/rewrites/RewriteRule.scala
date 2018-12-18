package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOne
import structures.HyperGraphManyWithOrderToOneLike._
import structures.immutable.VocabularyHyperGraph
import synthesis.rewrites.RewriteRule.{Category, HyperPattern, SubHyperEdgePattern, SubHyperGraphPattern}
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator
import synthesis.{HyperTerm, HyperTermIdentifier}

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

    def extractNewEdges(m: Map[Int, Either[HyperTerm, HyperTermIdentifier]]): Set[HyperEdge[Item[HyperTerm, Int], Item[HyperTermIdentifier, Int]]] = {
      m.foldLeft(subGraphDestination)((graph, kv) => {
        // From each map create new edges from the destination graph
        val ng: SubHyperGraphPattern = graph.mergeNodes(Explicit(kv._2.merge), Hole(kv._1))

        kv._2 match {
          case Right(k) => ng.mergeEdgeTypes(Explicit(k), Hole(kv._1))
          case _ => ng
        }
      }).edges
    }

    def extract[T](i: Item[T, Int]): T = i match {
      case Explicit(v) => v
    }

    // Should crash if we still have holes as its a bug
    val graph = compactGraph.addEdges(conditionsReferencesMaps.flatMap{extractNewEdges}.map(e =>
      HyperEdge(extract[HyperTerm](e.target), extract[HyperTermIdentifier](e.edgeType), e.sources.map(extract[HyperTerm]))
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

  private def termToHyperItem(templateTerm: TemplateTerm): Item[HyperTerm, Int] = {
    case ReferenceTerm(i) => Hole(i)
    case ExplicitTerm(term) => Explicit(term)
  }

  private def termToHyperIdentifierItem(templateTerm: TemplateTerm): Item[HyperTermIdentifier, Int] = {
    case ReferenceTerm(i) => Hole(i)
    case ExplicitTerm(term) => Explicit(term)
  }

  private lazy val subGraphConditions: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = conditions.edges.map(e =>
      HyperEdge[Item[HyperTerm, Int], Item[HyperTermIdentifier, Int]](termToHyperItem(e.target), termToHyperIdentifierItem(e.edgeType), e.sources.map(termToHyperItem))
    )
    VocabularyHyperGraph[Item[HyperTerm, Int], Item[HyperTermIdentifier, Int]](edges).asInstanceOf[SubHyperGraphPattern]
  }

  private lazy val subGraphDestination: SubHyperGraphPattern = {
    val edges: Set[SubHyperEdgePattern] = destination.edges.map(e =>
      HyperEdge[Item[HyperTerm, Int], Item[HyperTermIdentifier, Int]](termToHyperItem(e.target), termToHyperIdentifierItem(e.edgeType), e.sources.map(termToHyperItem))
    )
    VocabularyHyperGraph[Item[HyperTerm, Int], Item[HyperTermIdentifier, Int]](edges).asInstanceOf[SubHyperGraphPattern]
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPattern = HyperGraphManyWithOrderToOne[TemplateTerm, TemplateTerm]

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }

  def createHyperPatternFromTemplates(templates: Seq[Template]): HyperPattern = {
    templates.foldLeft[HyperPattern](VocabularyHyperGraph.empty)((graph, pattern) =>
      graph.addEdge(HyperEdge[TemplateTerm, TemplateTerm](pattern.target, pattern.function, pattern.parameters))
    )
  }

  /* --- Privates --- */

  private type SubHyperGraphPattern = HyperGraphPattern[HyperTerm, HyperTermIdentifier, Int, SubHyperGraphPattern]
  private type SubHyperEdgePattern = HyperEdgePattern[HyperTerm, HyperTermIdentifier, Int]
}
