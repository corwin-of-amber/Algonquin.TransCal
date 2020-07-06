package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import synthesis.Programs.NonConstructableMetadata
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, RewriteRule, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class LetActionTest extends FunSuite with Matchers with LazyLogging {

  test("Bidirectional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat = ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(AnnotatedTree.identifierOnly(Identifier("concat"))), Set.empty[RewriteRule])
    newState.rewriteRules.size shouldEqual 3
  }

  test("Directional let get correct amount of rewrites") {
    val letTerm = (new TranscalParser).apply("concat >> ?xs :: ?xss ↦ xs ++ concat xss")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(AnnotatedTree.identifierOnly(Identifier("concat"))), Set.empty[RewriteRule])
    newState.rewriteRules.size shouldEqual 2
  }

  test("Simple let rewrite should match and reconstruct") {
    val letTerm = (new TranscalParser).apply("f ?x >> x + y")
    val newState = new LetAction(letTerm) apply new ActionSearchState(Programs(AnnotatedTree(Identifier("f"), List(AnnotatedTree.identifierOnly(Identifier("z"))), Seq.empty)), Set.empty[RewriteRule])
    newState.rewriteRules.size shouldEqual 1
    newState.updateGraph(g => newState.rewriteRules.head(g))
    val newEdges = newState.programs.queryGraph.findEdges(HyperTermIdentifier(Identifier("+")))
    newEdges.size shouldEqual 1
    newState.programs.reconstruct(newEdges.head.target).toSeq should contain((new TranscalParser).apply("_ -> z + y").subtrees(1))
  }

  test("Handles precondition correctly") {
    val letTerm = (new TranscalParser).apply("(?x ≤ ?y) ||> min(x, y) >> id x")
    val letAction = new LetAction(letTerm)
    val a = Identifier("a")
    val b = Identifier("b")
    val minTree = AnnotatedTree(Identifier("min"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty)
    val newState = letAction apply new ActionSearchState(Programs(AnnotatedTree(
      Language.trueCondBuilderId,
      List(
        AnnotatedTree(Identifier("≤"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty),
        AnnotatedTree(Identifier("min"), List(AnnotatedTree.identifierOnly(a), AnnotatedTree.identifierOnly(b)), Seq.empty)
      ),
      Seq.empty)
    ), Set.empty[RewriteRule])
    newState.rewriteRules.size shouldEqual 1
    newState.updateGraph(g => newState.rewriteRules.head(g))
    val aEdge = newState.programs.queryGraph.findEdges(HyperTermIdentifier(a)).head
    newState.programs.reconstruct(aEdge.target).contains(minTree) shouldEqual true
  }

  test("rewriteRules can rewrite correct matches") {
    val term = new TranscalParser().apply("f >> true match (true ⇒ hello / false => world)")
    val (graph, _) = Programs.destructWithRoot(term)
    val letAction = new LetAction(term)
    for (_ <- 0 to 4; r <- letAction.rules) r(graph)
    val fRoot = graph.findRegex(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("f"))), List(), EmptyMetadata)).head.edges.head.target
    graph.exists(e => e.target == fRoot && e.edgeType.identifier.literal.toString == "hello") shouldEqual true
  }

  test("can create correct reverse rewrites") {
    val term = new TranscalParser().apply("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")
    val letAction = new LetAction(term)
    val (graph, root) = Programs.destructWithRoot(new TranscalParser().parseExpression("reverse (x::y::nil)"))
    val anchor = HyperTermIdentifier(Identifier("anchor"))
    var state = graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata)
    for (_ <- 0 to 4;
         r <- letAction.rules ++ SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules) {
      r(state)
    }
    val newRoot = state.findEdges(anchor).head.target
    val resPattern = Programs.destructPattern(new TranscalParser().parseExpression("y :: _"))
    val terms = Programs(state).reconstructWithPattern(newRoot, resPattern).take(100).toSeq
    terms should not be empty
  }
}
