package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import structures.HyperEdge
import synthesis.actions.ActionSearchState
import synthesis._
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.{Identifier, TranscalParser}

class CaseSplitActionTest extends FunSuite with Matchers with ParallelTestExecution {
  val parser = new TranscalParser
  val normalRules = SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules

  test("test splitting in depth returns only known ids") {
    val tree = parser parseExpression "(splitTrue ||| possibleSplit(x, 2, 2) ||| possibleSplit(y, ⟨⟩, ⟨⟩)) |||| x :: y |||| 2 :: ⟨⟩"
    val progs = Programs(tree)
    val res = new CaseSplitAction(None, Some(2), None).getFoundConclusions(ActionSearchState(progs, normalRules))
    res.flatten shouldEqual progs.hyperGraph.nodes
    val mutableRes = new CaseSplitAction(None, Some(2), None).getFoundConclusionsFromRewriteState(new RewriteSearchState(progs.hyperGraph), normalRules)
    mutableRes.flatten shouldEqual progs.hyperGraph.nodes
  }

  test("test splitting to true and false finds it is the same") {
    val state1 = new DefAction(parser apply "whatever ?x = (x match ((true => 5) / (false => 5)))")(ActionSearchState(Programs.empty,
      normalRules))
    val state = state1.copy(programs = state1.programs.addTerm(parser parseExpression "whatever z"))
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("5"))).head

    val res = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), pattern, root)(state)
    assert(state == res)
    val splitableState = ActionSearchState(
      state.programs.addTerm(parser parseExpression s"${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}(z, true, false)"),
      state.rewriteRules
    )
    val res2 = new CaseSplitAction(splitableState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).head).getFoundConclusions(splitableState)
    val newState = ObservationalEquivalence.mergeConclusions(new RewriteSearchState(splitableState.programs.hyperGraph), res2.toSeq)
    newState.graph.findSubgraph[Int](Programs.destructPattern(parser parseExpression "whatever z ||| 5" map (_.copy(annotation = None)))).nonEmpty shouldBe true
  }

  test("test splitting examples for rev snoc vs 3 elaborate") {
    // First elaborate each case seperatly
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val rules = reverseRules ++ SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules
    val nilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse(⟨⟩ :+ z)").addTerm(parser parseExpression "z :: reverse(⟨⟩)"), rules)
    val (nilPattern, nilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(⟨⟩)")).head
    val nilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), nilPattern, nilRoot, maxSearchDepth = Some(8))(nilState)
    nilResState should not equal (nilState)
    val xNilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse((x::⟨⟩) :+ z)").addTerm(parser parseExpression "z :: reverse(x::⟨⟩)"), rules)
    val (xNilPattern, xNilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(x::⟨⟩)")).head
    val xNilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), xNilPattern, xNilRoot, maxSearchDepth = Some(8))(xNilState)
    xNilResState should not equal (xNilState)
    val yxNilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse((y::x::⟨⟩) :+ z)").addTerm(parser parseExpression "z :: reverse(y::x:::⟨⟩)"), rules)
    val (yxNilPattern, yxNilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(y::x::⟨⟩)")).head
    val yxNilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), yxNilPattern, yxNilRoot, maxSearchDepth = Some(8))(nilState)
    yxNilResState should not equal (yxNilState)

    // Now run casesplit and check all cases are matching
    val splitter = parser parseExpression "possibleSplit(l, ⟨⟩, x::⟨⟩, y::x::⟨⟩)"
    val splitterState = new ActionSearchState(Programs.empty
      .addTerm(parser parseExpression "a1 ||| reverse(l :+ z)")
      .addTerm(parser parseExpression "a2 ||| z :: reverse(l)")
      .addTerm(parser parseExpression "possibleSplit(l, ⟨⟩, x::⟨⟩, y::x::⟨⟩)"), rules)
    val conclusions = new CaseSplitAction(None, None, Some(6)).getFoundConclusions(splitterState)
    val correctId1 = splitterState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(Identifier("a1"))).head.target
    val correctId2 = splitterState.programs.hyperGraph.findByEdgeType(HyperTermIdentifier(Identifier("a2"))).head.target
    conclusions.find(_.contains(correctId1)).get should contain (correctId2)
  }

  test("test splitting filter p filter p l") {
    var state = new DefAction(parser apply "filter p (filter p (x :: y :: ⟨⟩)) = filter p (filter p l)")(ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    val res = new ObservationalEquivalenceWithCaseSplit(8, splitDepth = Some(3)).getEquives(state)
//    val res = new CaseSplitAction(splitterChooser = None, splitDepthOption = Some(3), maxDepthOption = Some(8))
//      .getFoundConclusions(state)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (filter p (x :: y :: ⟨⟩))" map (_.copy(annotation = None)))).head
    val patternResults = state.programs.hyperGraph.findSubgraph[Int](pattern)
    patternResults should not be empty
    val correctId = patternResults.head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    val correctSet = res.find(_.contains(correctId))
    correctSet should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (x :: y :: ⟨⟩)" map (_.copy(annotation = None)))).head
    val correctId2 = state.programs.hyperGraph.findSubgraph[Int](pattern2).head._1(root2.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    correctSet.get should contain (correctId2)
  }

  test("test splitting filter p filter q") {
    var state = new DefAction(parser apply "filter q (filter p (x :: y :: ⟨⟩)) = filter q (filter p l)")(ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    state = new DefAction(parser apply "filter p (filter q (x :: y :: ⟨⟩)) = filter p (filter q l)")(state)
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    state = new OperatorRunAction(maxSearchDepth = 2)(state)
    val res = new CaseSplitAction(splitterChooser = None, splitDepthOption = Some(4), maxDepthOption = Some(4))
      .getFoundConclusions(state)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (filter q (x :: y :: ⟨⟩))" map (_.copy(annotation = None)))).head
    val patternResults = state.programs.hyperGraph.findSubgraph[Int](pattern)
    patternResults should not be empty
    val correctId = patternResults.head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    val correctSet = res.find(_.contains(correctId))
    correctSet should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter q (filter p (x :: y :: ⟨⟩))" map (_.copy(annotation = None)))).head
    val correctId2 = state.programs.hyperGraph.findSubgraph[Int](pattern2).head._1(root2.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    correctSet.get should contain (correctId2)
  }

  test("test can't split on same edge twice") {
    class Chooser() {
      var called: Int = 0
      val chooser = CaseSplitAction.randomChooser(2, 2)
      def choose(state: RewriteSearchState, chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]):
        Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
        val res = chooser(state, chosen)
        if (res.nonEmpty) {
          called += 1
        }
        res
      }
    }
    val chooser = new Chooser()
    val caseSplitAction = new CaseSplitAction(Some(chooser.choose), Some(2), Some(1))
    val state = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "possibleSplit(true, true, false)"),
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    caseSplitAction(state)
    chooser.called shouldEqual 1
  }
}
