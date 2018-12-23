package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.immutable.HyperGraphManyWithOrderToOne.HyperEdge
import structures.immutable.HyperGraphManyWithOrderToOne
import syntax.Identifier
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}
import synthesis.rewrites.RewriteRule.{Category, HyperPattern}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

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

  private def mapper[T <: HyperTerm](creator: Iterator[T]): Template.TemplateTerm => T = {
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


  property("Every state adds edges") {
    check(forAll { (conditions: HyperPattern, destinations: HyperPattern, ruleType: Category.Value) => {
      val rewriteRule = new RewriteRule(conditions, destinations, ruleType)
      val templateTermToHyperTermId: Template.TemplateTerm => HyperTermId = mapper(Stream.from(0).map(HyperTermId).iterator)
      val templateTermToHyperTermIdentifier: Template.TemplateTerm => HyperTermIdentifier = mapper(Stream.from(0).map(new Identifier(_)).map(HyperTermIdentifier).iterator)
      val state = new RewriteSearchState(HyperGraphManyWithOrderToOne[HyperTermId, HyperTermIdentifier](conditions.edges.map(edge => {
        HyperEdge[HyperTermId, HyperTermIdentifier](templateTermToHyperTermId(edge.target), templateTermToHyperTermIdentifier(edge.edgeType), edge.sources.map(templateTermToHyperTermId))
      })))
      val newState = rewriteRule.apply(state)
      (newState.graph.edges -- state.graph.edges).size == (destinations.edges -- conditions.edges).size
    }
    })
  }
}
