package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.immutable.{Explicit, Item, Reference}
import structures.mutable.VocabularyHyperGraph
import structures.HyperGraphManyWithOrderToOne
import synthesis.Term
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(destination: Template, hyperPattern: HyperGraphManyWithOrderToOne[Item[Term, Int], Item[Term, Int]], ruleType: RewriteRule.Category.Value) extends Operator[RewriteSearchState] with LazyLogging {

  def this(source: Template, destination: Template, conditions: Seq[Template], ruleType: RewriteRule.Category.Value) = {
    this(destination, RewriteRule.createHyperPatternFromTemplate(source +: conditions), ruleType)
  }

  /* --- Operator Impl. --- */

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsAndSourceReferencesMaps: Set[Map[TemplateTerm, Either[Term, Term]]] = compactGraph.findSubgraph[TemplateTerm, HyperGraphManyWithOrderToOne[Item[Term, Int], Item[Term, Int]]](hyperPattern)
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

    new RewriteSearchState(state.hyperTerm, graph)
  }


  /* --- Privates --- */

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

  private def templateToPattern(references: Map[TemplateTerm, Term], template: Template): (Item[Term, Int], Item[Term, Int], Seq[Item[Term, Int]]) = {
    def templateTermToItem(templateTerm: TemplateTerm): Item[Term, Int] = {
      templateTerm match {
        case ReferenceTerm(id) => references.get(templateTerm).map(Explicit[Term, Int]).getOrElse(Reference[Term, Int](id))
        case ExplicitTerm(term) => Explicit[Term, Int](term)
      }
    }
    (templateTermToItem(template.target), templateTermToItem(template.function), template.parameters.map(templateTermToItem))
  }

  private def createHyperPatternFromTemplate(templates: Seq[Template]): HyperGraphManyWithOrderToOne[Item[Term, Int], Item[Term, Int]] = {
    def innerTemplateToPattern(templateTerm: Template): (Item[Term, Int], Item[Term, Int], Seq[Item[Term, Int]]) = templateToPattern.curried(Map.empty)

    templates.map(innerTemplateToPattern)
      .foldLeft[HyperGraphManyWithOrderToOne[Item[Term, Int], Item[Term, Int]]](VocabularyHyperGraph.empty[Item[Term, Int],
      Item[Term, Int]])((graph, pattern) => graph.addEdge(pattern))
  }
}
