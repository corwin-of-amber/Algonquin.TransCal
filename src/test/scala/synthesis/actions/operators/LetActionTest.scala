package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import transcallang.{Identifier, Language, TranscalParser}
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, Explicit, Hole, HyperEdge}
import syntax.Tree
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.{HyperTermIdentifier, Programs}

class LetActionTest extends FunSuite with Matchers with LazyLogging {

  test("Bidirectional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat = ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(new Tree(Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 3
  }

  test("Directional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(new Tree(Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 2
  }

  test("Simple let rewrite should match and reconstruct") {
    val letTerm = (new TranscalParser).apply("f ?x >> x + y")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(new Tree(Identifier("f"), List(new Tree(Identifier("z"))))), Set.empty)
    newState.rewriteRules.size shouldEqual 1
    val searchState = newState.rewriteRules.head.apply(new RewriteSearchState(newState.programs.hyperGraph))
    val newEdges = searchState.graph.findEdges(HyperTermIdentifier(Identifier("+")))
    newEdges.size shouldEqual 1
    Programs(searchState.graph).reconstruct(newEdges.head.target).toSeq should contain ((new TranscalParser).apply("_ -> z + y").subtrees(1))
  }

  test("Handles precondition correctly") {
    val letTerm = (new TranscalParser).apply("(?x ≤ ?y) ||> min(x, y) >> id x")
    val letAction = new LetAction(letTerm)
    val newState = letAction apply ActionSearchState(Programs(new Tree(Language.trueCondBuilderId, List(
      new Tree(Identifier("≤"), List(new Tree(Identifier("a")), new Tree(Identifier("b")))),
      new Tree(Identifier("min"), List(new Tree(Identifier("a")), new Tree(Identifier("b")))))
    )), Set.empty)
    newState.rewriteRules.size shouldEqual 1
    val searchState = newState.rewriteRules.head.apply(new RewriteSearchState(newState.programs.hyperGraph))
    val newEdges = searchState.graph.findEdges(HyperTermIdentifier(Language.idId))
    newEdges.size shouldEqual 1
  }

  test("rewriteRules can rewrite correct matches") {
    val term = new TranscalParser().apply("f >> true match (true ⇒ hello / false => world)")
    val (graph, root) = Programs.destructWithRoot(term)
    var state = new RewriteSearchState(graph)
    val letAction = new LetAction(term)
    for(i <- 0 to 4; r <- letAction.rules) state = r(state)
    val fRoot = state.graph.find(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("f"))), List(), EmptyMetadata)).head.target
    state.graph.exists(e => e.target == fRoot && e.edgeType.identifier.literal.toString == "hello") shouldEqual true
  }
}
