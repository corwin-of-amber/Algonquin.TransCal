package synthesis.actions.operators.SPBE

import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.operators.LetAction
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, Programs, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class SPBEActionTest extends FunSuite with Matchers {

  val listInt = AnnotatedTree.withoutAnnotations(Language.typeListId, Seq(Language.typeInt))
  val listInttoListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, listInt))
  val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
  val l = AnnotatedTree.identifierOnly(Identifier("l", Some(listInt)))
  val reverse = AnnotatedTree.identifierOnly(Identifier("reverse", annotation = Some(listInttoListInt)))
  val tru = AnnotatedTree.identifierOnly(Language.trueId)
  val fals = AnnotatedTree.identifierOnly(Language.falseId)

  test("testSygusStep can find reverse l") {
    val action = new SPBEAction(constantTeminals = Set(tru, fals, x), changingTerminals = Seq(Seq(l)), symbols = Set(reverse))
    val state = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val pattern = Programs.destructPattern(new TranscalParser().parseExpression("(reverse: (list int) :> (list int)) _"))
    state.graph.findSubgraph[Int](pattern) should not be empty
  }

  test("testSygusStep can find reverse reverse l") {
    val action = new SPBEAction(constantTeminals = Set(tru, fals, x), changingTerminals = Seq(Seq(l)), symbols = Set(reverse))
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("(reverse: (list int) :> (list int)) _"))).head
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("reverse (reverse _)"))).head
    val results1 = state2.graph.findSubgraph[Int](pattern1)
    val results2 = state2.graph.findSubgraph[Int](pattern2)
    results1 should not be empty
    results2 should not be empty
    val results2Roots = results2.map(_._1(root2.asInstanceOf[ReferenceTerm[HyperTermId]].id)).map(_.id)
    val results1Roots = results1.map(_._1(root1.asInstanceOf[ReferenceTerm[HyperTermId]].id)).map(_.id)
    results1Roots.diff(results2Roots) should not be empty
  }

  test("test find that l == reverse reverse l") {
    val action = new SPBEAction(constantTeminals = Set(tru, fals, x), changingTerminals = Seq(Seq(l)), symbols = Set(reverse))
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val filterRules = new LetAction(new TranscalParser()("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => ( (p x) match (true => x :: (filter p xs)) / false => filter p xs) ))")).rules
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => x :: (reverse xs)))")).rules
    val equives = action.findEquives(state2, (0 to 8) flatMap (_ => AssociativeRewriteRulesDB.rewriteRules.toSeq ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules))
    equives should not be empty
    equives.forall({case (k, v) => state2.graph.nodes.contains(k)}) should be (true)
    equives.flatMap(_._2).forall(k => state2.graph.nodes.contains(k)) should be (true)
    // TODO check equives contains reverese reverse to placeholder
  }
}
