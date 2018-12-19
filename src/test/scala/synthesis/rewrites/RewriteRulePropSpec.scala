package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
/**
  * @author tomer
  * @since 12/18/18
  */
class RewriteRulePropSpec extends PropSpec with Checkers {
  implicit val rewriteRuleCreator = Arbitrary(rewriteRuleGen)
  implicit val rewriteSearchStateCreator = Arbitrary(rewriteSearchStateGen)

  property("Every state keep old edges") {
    check(forAll { (rewriteRule: RewriteRule, state: RewriteSearchState)  => {
      val newState = rewriteRule.apply(state)
      (state.graph.edges -- newState.graph.edges).isEmpty
    }
    })
  }
}
