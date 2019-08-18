package synthesis.actions.operators.SPBE

import org.scalatest.{FunSuite, Matchers}
import structures.immutable
import synthesis.actions.operators.LetAction
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, Programs, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class SPBEActionTest extends FunSuite with Matchers {

  val listInt = AnnotatedTree.withoutAnnotations(Language.typeListId, Seq(Language.typeInt))
  val listInttoListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, listInt))
  val inttoListIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, listInt, listInt))
  val listIntToIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, Language.typeInt, listInt))
  val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
  val y = AnnotatedTree.identifierOnly(Identifier("y", Some(Language.typeInt)))
  val typedCons = Language.consId.copy(annotation = Some(inttoListIntToListInt))
  val typedSnoc = Language.snocId.copy(annotation = Some(listIntToIntToListInt))
  val nil = AnnotatedTree.identifierOnly(Language.nilId.copy(annotation = Some(listInt)))
  val xnil = AnnotatedTree.withoutAnnotations(typedCons, Seq(x, nil))
  val xynil = AnnotatedTree.withoutAnnotations(typedCons, Seq(y, xnil))
  val reverse = AnnotatedTree.identifierOnly(Identifier("reverse", annotation = Some(listInttoListInt)))
  val tru = AnnotatedTree.identifierOnly(Language.trueId)
  val fals = AnnotatedTree.identifierOnly(Language.falseId)

  test("testSygusStep can find reverse l") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, x, y), examples = Map(listInt -> Seq(nil, xnil, xynil)))
    val state = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val pattern = Programs.destructPattern(new TranscalParser().parseExpression("(reverse: (list int) :> (list int)) _"))
    state.graph.findSubgraph[Int](pattern) should not be empty
  }

  test("testSygusStep can find reverse reverse l") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, x, y), examples = Map(listInt -> Seq(nil, xnil, xynil)))
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
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, x, y), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 8)
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val equives = action.findEquives(state2, AssociativeRewriteRulesDB.rewriteRules.toSeq ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    equives should not be empty
    equives.forall({s => s.forall(state2.graph.nodes.contains)}) should be (true)
    val programs = Programs(state2.graph)
    val terms = equives.map(s => s.map(id => programs.reconstruct(id).toSeq))
    val correctSet = terms.map(_.map(_.toList)).find(s => s.exists(_.exists(_.root.literal=="Placeholder(0) type(list(int))")))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    val reversePlaceholderTwice = correctSet.get.exists(_.exists(t => t.root.literal=="reverse" && t.subtrees.head.root.literal=="reverse" && t.subtrees.head.subtrees.head.root.literal == "Placeholder(0)"))
   reversePlaceholderTwice shouldEqual true
  }
}
