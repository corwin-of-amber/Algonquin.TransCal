package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge}
import syntax.Identifier
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 12/18/18
  */
class RewriteRulePropSpec extends PropSpec with Checkers {
  private implicit val ruleTypeCreator = Arbitrary(ruleTypeGen)
  private implicit val hyperPatternCreator = Arbitrary(hyperPatternGen)
  private implicit val rewriteRuleCreator = Arbitrary(rewriteRuleGen)
  private implicit val rewriteSearchStateCreator = Arbitrary(rewriteSearchStateGen)

  property("Every state keep old edges") {
    check(forAll { (rewriteRule: RewriteRule, state: RewriteSearchState)  => {
      val newState = rewriteRule.apply(state)
      (state.graph.edges -- newState.graph.edges).isEmpty
    }
    })
  }

  property("Every state adds edges") {
    check(forAll { (conditions: HyperPattern, destinations: HyperPattern) => {
      val rewriteRule = new RewriteRule(conditions, destinations, (a, b) => EmptyMetadata)
      val templateTermToHyperTermId: Template.TemplateTerm => HyperTermId = RewriteRulePropSpec.mapper(Stream.from(0).map(HyperTermId).iterator)
      val templateTermToHyperTermIdentifier: Template.TemplateTerm => HyperTermIdentifier = RewriteRulePropSpec.mapper(Stream.from(0).map(new Identifier(_)).map(HyperTermIdentifier).iterator)
      val state = new RewriteSearchState(HyperGraphManyWithOrderToOne[HyperTermId, HyperTermIdentifier](conditions.edges.map(edge => {
        HyperEdge[HyperTermId, HyperTermIdentifier](templateTermToHyperTermId(edge.target), templateTermToHyperTermIdentifier(edge.edgeType), edge.sources.map(templateTermToHyperTermId), EmptyMetadata)
      })))
      val newState = rewriteRule.apply(state)
      (newState.graph.edges -- state.graph.edges).size == (destinations.edges -- conditions.edges).size
    }
    })
  }
}

object RewriteRulePropSpec {
  def mapper[T <: HyperTerm](creator: Iterator[T]): Template.TemplateTerm => T = {
    val intToT = scala.collection.mutable.Map.empty[Int, T]
    def templateTermToT(templateTerm: TemplateTerm): T = {
      templateTerm match {
        case ReferenceTerm(id) =>
          if (!intToT.contains(id)) {
            intToT(id) = creator.next()
          }
          intToT(id)
        case ExplicitTerm(hyperTerm) => hyperTerm.asInstanceOf[T]
      }
    }
    templateTermToT
  }
}
