package synthesis.rewrites

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOne
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import structures.immutable.Item
import synthesis.Term
import synthesis.rewrites.Template.{ReferenceTerm, TemplateTerm}
import synthesis.search.Operator

/**
  * @author tomer
  * @since 11/18/18
  */
class RewriteRule(source: Template, destination: Template, conditions: Seq[Template], val ruleType: RewriteRule.Category.Value) extends Operator[RewriteSearchState] with LazyLogging {


  /* --- Operator Impl. --- */

  override def apply(state: RewriteSearchState): RewriteSearchState = {
    val compactGraph = compact(state.graph) // Should this be only after id ruleType? we don't really need to compact any other time!

    // Fill conditions - maybe subgraph matching instead of current temple

    val conditionsAndSourceReferencesMaps = RewriteRule.getReferencesMap(compactGraph, conditions :+ source, Map.empty[TemplateTerm, Term])
    val r = (for (conditionsAndSourceReferencesMap <- conditionsAndSourceReferencesMaps) yield {
        val destinationPattern = RewriteRule.templateToPattern(destination, conditionsAndSourceReferencesMap)

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

  private def getReferencesMap(graph: HyperGraphManyWithOrderToOne[Term, Term], conditions: Seq[Template], referencesMap: Map[TemplateTerm, Term]): Stream[Map[TemplateTerm, Term]] = {
    conditions match {
      case Nil => Stream(referencesMap)
      case condition::left => {
        (for (HyperEdge(target, function, parameters) <- graph.find(RewriteRule.templateToPattern(condition))) yield {
          val newReferences = RewriteRule.hyperEdgeAndTemplateToReferencesMap(target, function, parameters, condition)
          getReferencesMap(graph, left, newReferences)
        }).flatten.toStream
      }
    }
  }

  private def templateToPattern(template: Template, references: Map[TemplateTerm, Term]=Map.empty): (Item[Term, Int], Item[Term, Int], Seq[Item[Term, Int]])= {
    def templateTermToItem(templateTerm: TemplateTerm): Item[Term, Int] = {
//      templateTerm match {
//        case ExplicitTerm(term) => Explicit[Term, Int](term)
//        case ReferenceTerm(id) => references.get(templateTerm).map(Explicit[Term, Int]).getOrElse(Reference[Term, Int](id))
//      }
      null
    }
    (templateTermToItem(template.target), templateTermToItem(template.function), template.parameters.map(templateTermToItem))
  }

  private def hyperEdgeAndTemplateToReferencesMap(target: Term, function: Term, parameters: Seq[Term], template: Template): Map[TemplateTerm, Term] = {
    val termToTemplate = (template.target, target) +: (template.function, function) +: (template.parameters zip parameters)
    termToTemplate.filter(a => a._1.isInstanceOf[ReferenceTerm]).toMap
  }
}
