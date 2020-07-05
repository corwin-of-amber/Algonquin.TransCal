package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import transcallang.TranscalParser
import org.scalatest.{FunSuite, Matchers}
import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.RewriteRule

class DefActionTest extends FunSuite with Matchers with LazyLogging {
  test("Directional def removes ? from vars") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new DefAction(letTerm) apply new ActionSearchState(Programs.empty, Set.empty[RewriteRule])
    newState.programs.queryGraph.size shouldNot be (0)
    newState.programs.queryGraph.edgeTypes.map(_.identifier.literal.toString).count(_.startsWith("?")) shouldEqual 0
  }
}
