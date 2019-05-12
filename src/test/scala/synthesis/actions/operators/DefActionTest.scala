package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import transcallang.TranscalParser
import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis.Programs

class DefActionTest extends FunSuite with Matchers with LazyLogging {
  test("Directional def removes ? from vars") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new DefAction(letTerm) apply ActionSearchState(Programs.empty, Set.empty)
    newState.programs.hyperGraph.size shouldNot be (0)
    newState.programs.hyperGraph.edgeTypes.map(_.identifier.literal.toString).count(_.startsWith("?")) shouldEqual 0
  }
}
