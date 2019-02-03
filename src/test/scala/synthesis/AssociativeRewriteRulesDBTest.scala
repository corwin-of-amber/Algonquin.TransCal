package synthesis

import language.TranscalParser
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import synthesis.rewrites.RewriteSearchState

class AssociativeRewriteRulesDBTest extends PropSpec with Checkers {

  property("rewriteRules manage to rewrite a + b + c to (a + b) + c") {
    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm, Set.empty)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = AssociativeRewriteRulesDB.rewriteRules
    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  }

}
