package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import transcallang.TranscalParser
import org.scalatest.{FunSuite, Matchers}
import structures.immutable.VocabularyHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import syntax.{Identifier, Tree}
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

class DefActionTest extends FunSuite with Matchers with LazyLogging {
  test("Directional def removes ? from vars") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new DefAction(letTerm) apply ActionSearchState(Programs.empty, Set.empty)
    newState.programs.hyperGraph.size shouldNot be (0)
    newState.programs.hyperGraph.edgeTypes.map(_.identifier.literal.toString).count(_.startsWith("?")) shouldEqual 0
  }
}
