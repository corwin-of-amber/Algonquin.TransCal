package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOne
import structures.HyperGraphManyWithOrderToOneLike._
import structures.immutable.VocabularyHyperGraph
import synthesis.rewrites.RewriteRule.{Category, HyperPattern, HyperPatternEdge}
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

    def merge(either: Either[HyperTerm, HyperTerm]): HyperTerm = {
      either match {
        case Left(left) => left
        case Right(right) => right
      }
    }

    val conditionsReferencesMaps = compactGraph.findSubgraph[TemplateTerm, HyperPattern](conditions).map(_.map(kv => (kv._1, merge(kv._2))))

    def extract(i: Item[HyperTerm, TemplateTerm]): HyperTerm = i match {
      case Explicit(v) => v
    }

    def extract2(i: Item[HyperTermIdentifier, TemplateTerm]): HyperTermIdentifier = i match {
      case Explicit(v) => v
    }

    def extractNewEdges(m: Map[Template.TemplateTerm, HyperTerm]): Set[HyperPatternEdge] = m.foldLeft(destination)((graph, kv) => {
      // From each map create new edges from the destination graph
      val ng = graph.mergeNodes(Explicit(kv._2), Hole(kv._1))
      kv._2 match {
        case k: HyperTermIdentifier => ng.mergeEdgeTypes(Explicit(k.asInstanceOf[HyperTermIdentifier]), Hole(kv._1))
        case _ => ng
      }
    }).edges

    val graph = conditionsReferencesMaps.flatMap {
      extractNewEdges
    }.map(e =>
      HyperEdge(extract(e.target), extract2(e.edgeType), e.sources.map(extract))
    ).foldLeft(VocabularyHyperGraph.empty[HyperTerm, HyperTermIdentifier])((g, e) => g.addEdge(e))

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
}

object RewriteRule {

  /* --- Public --- */

  type HyperPatternEdge = HyperEdgePattern[HyperTerm, HyperTermIdentifier, TemplateTerm]
  type HyperPattern = HyperGraphManyWithOrderToOne[Item[HyperTerm, TemplateTerm], Item[HyperTermIdentifier, TemplateTerm]]

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }


  /* --- Privates --- */

  private def createHyperPatternFromTemplate(templates: Seq[Template]): HyperPattern = {
    templates.map(templateToPattern(Map.empty, _))
      .foldLeft[HyperPattern](VocabularyHyperGraph.empty)((graph, pattern) => graph.addEdge(pattern))
  }

  private def templateToPattern(references: Map[TemplateTerm, HyperTerm], template: Template): HyperPatternEdge = {
    def templateTermToItem(templateTerm: TemplateTerm): Item[HyperTerm, TemplateTerm] = {
      templateTerm match {
        case term: ReferenceTerm =>
          references.get(templateTerm).map(Explicit[HyperTerm, TemplateTerm]).getOrElse(Hole[HyperTerm, TemplateTerm](term))
        case term: ExplicitTerm =>
          Explicit[HyperTerm, TemplateTerm](term.term)
      }
    }

    def templateTermToIdentifierItem(templateTerm: TemplateTerm): Item[HyperTermIdentifier, TemplateTerm] = {
      templateTerm match {
        case term: ReferenceTerm =>
          references.get(templateTerm).map(_.asInstanceOf[HyperTermIdentifier])
            .map(Explicit[HyperTermIdentifier, TemplateTerm]).getOrElse(Hole[HyperTermIdentifier, TemplateTerm](term))
        case term: ExplicitTerm =>
          Explicit[HyperTermIdentifier, TemplateTerm](term.term.asInstanceOf[HyperTermIdentifier])
      }
    }

    HyperEdge(templateTermToItem(template.target), templateTermToIdentifierItem(template.function),
      template.parameters.map(templateTermToItem))
  }
}
