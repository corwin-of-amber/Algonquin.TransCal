package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.{HyperTermIdentifier, Programs}
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
    Programs(searchState.graph).reconstruct(newEdges.head.target).toSeq should contain ((new TranscalParser).apply("_ -> z + y").subtrees(1))
  }

  test("Handles precondition correctly") {
    val letTerm = (new TranscalParser).apply("(?x ≤ ?y) ||> min(x, y) >> id x")
    val letAction = new LetAction(letTerm)
    val newState = letAction apply ActionSearchState(Programs(AnnotatedTree(
      Language.trueCondBuilderId,
      List(
        AnnotatedTree(Identifier("≤"), List(AnnotatedTree.identifierOnly(Identifier("a")), AnnotatedTree.identifierOnly(Identifier("b"))), Seq.empty),
        AnnotatedTree(Identifier("min"), List(AnnotatedTree.identifierOnly(Identifier("a")), AnnotatedTree.identifierOnly(Identifier("b"))), Seq.empty)
      ),
      Seq.empty)
    ), Set.empty)
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
    val fRoot = state.graph.findRegex(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("f"))), List(), EmptyMetadata)).head._1.target
    state.graph.exists(e => e.target == fRoot && e.edgeType.identifier.literal.toString == "hello") shouldEqual true
  }

//  test("rewriteRules can rewrite reverse function") {
//    val listInttoListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeList, Language.typeList))
//    val nil = AnnotatedTree.identifierOnly(Identifier("nil", Some(Language.typeListId)))
//    val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
//    val y = AnnotatedTree.identifierOnly(Identifier("y", Some(Language.typeInt)))
//    val xnil = AnnotatedTree.withoutAnnotations(Language.consId, Seq(x, nil))
//    val yxnil = AnnotatedTree.withoutAnnotations(Language.consId, Seq(y, xnil))
//    val term = new TranscalParser().apply("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => x :: (reverse xs)))")
//    val (graph, root) = Programs.destructWithRoot(AnnotatedTree.withoutAnnotations(Identifier("reverse"), Seq(yxnil)))
//    var state = new RewriteSearchState(graph)
//    val letAction = new LetAction(term)
//    for(i <- 0 to 4; r <- letAction.rules) state = r(state)
//    val fRoot = state.graph.findRegex(HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("f"))), List(), EmptyMetadata)).head._1.target
//    state.graph.exists(e => e.target == fRoot && e.edgeType.identifier.literal.toString == "hello") shouldEqual true
//  }
}
