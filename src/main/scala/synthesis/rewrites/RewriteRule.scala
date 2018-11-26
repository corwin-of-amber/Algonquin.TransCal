package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.immutable.{Explicit, Item, Reference}
import structures.mutable.VocabularyHyperGraph
import structures.{HyperEdge, HyperGraphManyWithOrderToOne}
import synthesis.Term
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(destination: Template, hyperPattern: HyperGraphManyWithOrderToOne[Item[Term, TemplateTerm], Item[Term, TemplateTerm]], ruleType: RewriteRule.Category.Value) extends Operator[RewriteSearchState] with LazyLogging {

  def this(source: Template, destination: Template, conditions: Seq[Template], ruleType: RewriteRule.Category.Value) = {
    this(destination, RewriteRule.createHyperPatternFromTemplate(source +: conditions), ruleType)
  }

  /* --- Operator Impl. --- */

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsAndSourceReferencesMaps: Set[Map[TemplateTerm, Either[Term, Term]]] = compactGraph.findSubgraph(hyperPattern)
    def merge(either: Either[Term, Term]): Term = {
      either match {
        case Left(left) => left
        case Right(right) => right
      }
    }
    val r = (for (conditionsAndSourceReferencesMap <- conditionsAndSourceReferencesMaps) yield {
        val destinationPattern = RewriteRule.templateToPattern(conditionsAndSourceReferencesMap.map(v=>(v._1, merge(v._2))), destination)

        compactGraph.find(destinationPattern)
    }).flatten

    val graph = r.foldLeft(compactGraph)((graph, edge) => graph.addEdge(edge))

    new RewriteSearchState(graph)
  }


  /* --- Privates --- */

  /**
    * This function should work only after "id" rule.
    * @param graph
    * @return
    */
  private def compact(graph: HyperGraphManyWithOrderToOne[Term, Term]): HyperGraphManyWithOrderToOne[Term, Term] = {
    graph
  }
}

object RewriteRule {

  /* --- Public --- */

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }


  /* --- Privates --- */

  private def templateToPattern(references: Map[TemplateTerm, Term], template: Template): HyperEdge[Item[Term, TemplateTerm], Item[Term, TemplateTerm]] = {
    def templateTermToItem(templateTerm: TemplateTerm): Item[Term, TemplateTerm] = {
      templateTerm match {
        case term: ReferenceTerm =>
          references.get(templateTerm).map(Explicit[Term, TemplateTerm]).getOrElse(Reference[Term, TemplateTerm](term))
        case term: ExplicitTerm =>
          Explicit[Term, TemplateTerm](term.term)
      }
    }
    HyperEdge(templateTermToItem(template.target), templateTermToItem(template.function), template.parameters.map(templateTermToItem))
  }

  private def createHyperPatternFromTemplate(templates: Seq[Template]): HyperGraphManyWithOrderToOne[Item[Term, TemplateTerm], Item[Term, TemplateTerm]] = {
    templates.map(templateToPattern(Map.empty,_))
      .foldLeft[HyperGraphManyWithOrderToOne[Item[Term, TemplateTerm], Item[Term, TemplateTerm]]](VocabularyHyperGraph.empty[Item[Term, TemplateTerm],
      Item[Term, TemplateTerm]])((graph, pattern) => graph.addEdge(pattern))
  }
}
