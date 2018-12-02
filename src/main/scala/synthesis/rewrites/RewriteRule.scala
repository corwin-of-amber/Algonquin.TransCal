package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.mutable.VocabularyHyperGraph
import structures.HyperGraphManyWithOrderToOne
import structures.HyperGraphManyWithOrderToOneLike.{Explicit, HyperEdge, Item, Reference}
import synthesis.{HyperTerm, HyperTermIdentifier}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator

/** Rewrites a program to a new program.
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(destination: Template, hyperPattern: RewriteRule.HyperPattern, ruleType: RewriteRule.Category.Value) extends Operator[RewriteSearchState] with LazyLogging {


  /* --- Constructors --- */

  def this(source: Template, destination: Template, conditions: Seq[Template], ruleType: RewriteRule.Category.Value) = {
    this(destination, RewriteRule.createHyperPatternFromTemplate(source +: conditions), ruleType)
  }


  /* --- Operator Impl. --- */

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    logger.trace("Creating a new state")
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsAndSourceReferencesMaps: Set[Map[TemplateTerm, Either[HyperTerm, HyperTermIdentifier]]] = compactGraph.findSubgraph(hyperPattern)
    def merge(either: Either[HyperTerm, HyperTerm]): HyperTerm = {
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

  /** This function should work only after "id" rule.
    * @param graph
    * @return
    */
  private def compact(graph: RewriteSearchState.HyperGraph): RewriteSearchState.HyperGraph = {
    logger.trace("Compacting graph")
    graph
  }
}

object RewriteRule {

  /* --- Public --- */

  type HyperPatternEdge = HyperEdge[Item[HyperTerm, TemplateTerm], Item[HyperTermIdentifier, TemplateTerm]]
  type HyperPattern = HyperGraphManyWithOrderToOne[Item[HyperTerm, TemplateTerm], Item[HyperTermIdentifier, TemplateTerm]]

  object Category extends Enumeration {
    val Basic, Associative, Goal, Locator, Definition, Existential = Value
  }


  /* --- Privates --- */

  private def templateToPattern(references: Map[TemplateTerm, HyperTerm], template: Template): HyperPatternEdge = {
    def templateTermToItem(templateTerm: TemplateTerm): Item[HyperTerm, TemplateTerm] = {
      templateTerm match {
        case term: ReferenceTerm =>
          references.get(templateTerm).map(Explicit[HyperTerm, TemplateTerm]).getOrElse(Reference[HyperTerm, TemplateTerm](term))
        case term: ExplicitTerm =>
          Explicit[HyperTerm, TemplateTerm](term.term)
      }
    }
    def templateTermToItem2(templateTerm: TemplateTerm): Item[HyperTermIdentifier, TemplateTerm] = {
      templateTerm match {
        case term: ReferenceTerm =>
          references.get(templateTerm).map(_.asInstanceOf[HyperTermIdentifier])
            .map(Explicit[HyperTermIdentifier, TemplateTerm]).getOrElse(Reference[HyperTermIdentifier, TemplateTerm](term))
        case term: ExplicitTerm =>
          Explicit[HyperTermIdentifier, TemplateTerm](term.term.asInstanceOf[HyperTermIdentifier])
      }
    }
    HyperEdge(templateTermToItem(template.target), templateTermToItem2(template.function), template.parameters.map(templateTermToItem))
  }

  private def createHyperPatternFromTemplate(templates: Seq[Template]): HyperPattern = {
    templates.map(templateToPattern(Map.empty,_))
      .foldLeft[HyperPattern](VocabularyHyperGraph.empty)((graph, pattern) => graph.addEdge(pattern))
  }
}
