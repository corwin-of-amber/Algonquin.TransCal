package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, HyperTermIdentifier, Programs, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class LetActionTest extends FunSuite with Matchers with LazyLogging {

  test("Bidirectional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat = ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(AnnotatedTree.identifierOnly(Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 3
  }

  test("Directional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(AnnotatedTree.identifierOnly(Identifier("concat"))), Set.empty)
    newState.rewriteRules.size shouldEqual 2
  }

  test("Simple let rewrite should match and reconstruct") {
    val letTerm = (new TranscalParser).apply("f ?x >> x + y")
    val newState = new LetAction(letTerm) apply ActionSearchState(Programs(AnnotatedTree(Identifier("f"), List(AnnotatedTree.identifierOnly(Identifier("z"))), Seq.empty)), Set.empty)
    newState.rewriteRules.size shouldEqual 1
    val searchState = newState.rewriteRules.head.apply(new RewriteSearchState(newState.programs.hyperGraph))
    val newEdges = searchState.graph.findEdges(HyperTermIdentifier(Identifier("+")))
    newEdges.size shouldEqual 1
    Programs(searchState.graph).reconstruct(newEdges.head.target).toSeq should contain((new TranscalParser).apply("_ -> z + y").subtrees(1))
  }

  test("Handles precondition correctly") {
    val letTerm = (new TranscalParser).apply("(?x ≤ ?y) ||> min(x, y) >> id x")
    val letAction = new LetAction(letTerm)
    val a = Identifier("a")
    val b = Identifier("b")
    val minTree = AnnotatedTree(Identifier("min"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty)
    val newState = letAction apply ActionSearchState(Programs(AnnotatedTree(
      Language.trueCondBuilderId,
      List(
        AnnotatedTree(Identifier("≤"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty),
        AnnotatedTree(Identifier("min"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty)
      ),
      Seq.empty)
    ), Set.empty)
    newState.rewriteRules.size shouldEqual 1
    val searchState = newState.rewriteRules.head.apply(new RewriteSearchState(newState.programs.hyperGraph))
    val aEdge = searchState.graph.findEdges(HyperTermIdentifier(a)).head
    Programs(searchState.graph).reconstruct(aEdge.target).contains(minTree) shouldEqual true
  }

  test("rewriteRules can rewrite correct matches") {
    val term = new TranscalParser().apply("f >> true match (true ⇒ hello / false => world)")
    val (graph, _) = Programs.destructWithRoot(term)
    var state = new RewriteSearchState(graph)
    val letAction = new LetAction(term)
    for (_ <- 0 to 4; r <- letAction.rules) state = r(state)
    val fRoot = state.graph.findRegex(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("f"))), List(), EmptyMetadata)).head._1.target
    state.graph.exists(e => e.target == fRoot && e.edgeType.identifier.literal.toString == "hello") shouldEqual true
  }

  test("can create correct reverse rewrites") {
    val term = new TranscalParser().apply("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")
    val letAction = new LetAction(term)
    val (graph, root) = Programs.destructWithRoot(new TranscalParser().parseExpression("reverse (x::y::nil)"))
    val anchor = HyperTermIdentifier(Identifier("anchor"))
    var state = new RewriteSearchState(graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata))
    for (_ <- 0 to 4;
         r <- letAction.rules ++ SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules) {
      state = r(state)
    }
    val newRoot = state.graph.findEdges(anchor).head.target
    val resPattern = Programs.destructPattern(new TranscalParser().parseExpression("y :: _"))
    val terms = Programs(state.graph).reconstructWithPattern(newRoot, resPattern).take(100).toSeq
    terms should not be empty
  }
}
