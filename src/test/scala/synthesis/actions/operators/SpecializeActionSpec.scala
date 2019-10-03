package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import synthesis.Programs
import synthesis.actions.ActionSearchState
import transcallang.TranscalParser

class SpecializeActionSpec extends FunSuite with Matchers {

  test("simple test") {
    val parser = new TranscalParser
    val searchTerm = parser.parseExpression("map f l")
    val functionOp = parser.parseExpression("f i")
    val newPreds = parser.parseExpression("i < l")
    val action = new SpecializeAction(searchTerm, functionOp, newPreds)

    val programs = Programs(parser.apply("f i = i + 1")).addTerm(parser.apply("g l = map f l"))
    val state = ActionSearchState(programs, Set.empty)

    val newState = action.apply(state)
    newState.programs.hyperGraph.size should be > programs.hyperGraph.size;
    newState.rewriteRules.nonEmpty should be (true)
  }
}
