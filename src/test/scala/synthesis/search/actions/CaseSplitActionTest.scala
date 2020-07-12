package synthesis.search.actions

import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import structures.HyperEdge
import synthesis.{search, _}
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.Template.ReferenceTerm
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{Identifier, TranscalParser}

class CaseSplitActionTest extends FunSuite with Matchers with ParallelTestExecution {
  val parser = new TranscalParser
  val normalRules = SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules
  val searcher = new OperatorRunAction(3)

  test("test splitting in depth returns only known ids") {
    val tree = parser parseExpression "(splitTrue ||| possibleSplit(x, 2, 2) ||| possibleSplit(y, ⟨⟩, ⟨⟩)) |||| x :: y |||| 2 :: ⟨⟩"
    val progs = Programs(tree)
    val res = new CaseSplitAction(searcher).getFoundConclusions(new ActionSearchState(progs, normalRules), 6)
    res.flatten shouldEqual progs.queryGraph.nodes
    val mutableRes = new CaseSplitAction(searcher).getFoundConclusions(new ActionSearchState(progs, normalRules), 6)
    mutableRes.flatten shouldEqual progs.queryGraph.nodes
  }

  test("test splitting to true and false finds it is the same") {
    val state1 = new DefAction(parser apply "whatever ?x = (x match ((true => 5) / (false => 5)))")(new ActionSearchState(Programs.empty,
      normalRules))
    val state = new search.ActionSearchState(state1.programs.addTerm(parser parseExpression "whatever z"), state1.rewriteRules)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("5"))).head

    val res = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), pattern, root)(state)
    assert(state == res)
    val splitableState = new ActionSearchState(
      state.programs.addTerm(parser parseExpression s"${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}(z, true, false)"),
      state.rewriteRules
    )
    val res2 = new CaseSplitAction(searcher, splitableState.programs.queryGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).head).getFoundConclusions(splitableState, 6)
    val newState = ObservationalEquivalence.mergeConclusions(splitableState, res2.toSeq)
    newState.programs.queryGraph.findSubgraph[Int](Programs.destructPattern(parser parseExpression "whatever z ||| 5" cleanTypes)).nonEmpty shouldBe true
  }

  test("test splitting examples for rev snoc vs 3 elaborate") {
    // First elaborate each case seperatly
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val rules = reverseRules ++ SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules
    val nilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse(⟨⟩ :+ z)").addTerm(parser parseExpression "z :: reverse(⟨⟩)"), rules)
    val (nilPattern, nilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(⟨⟩)")).head
    val nilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), nilPattern, nilRoot, maxSearchDepth = Some(8))(nilState.deepCopy())
    nilResState should not equal (nilState)
    val xNilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse((x::⟨⟩) :+ z)").addTerm(parser parseExpression "z :: reverse(x::⟨⟩)"), rules)
    val (xNilPattern, xNilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(x::⟨⟩)")).head
    val xNilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), xNilPattern, xNilRoot, maxSearchDepth = Some(8))(xNilState.deepCopy())
    xNilResState should not equal (xNilState)
    val yxNilState = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "a1 ||| reverse((y::x::⟨⟩) :+ z)").addTerm(parser parseExpression "z :: reverse(y::x:::⟨⟩)"), rules)
    val (yxNilPattern, yxNilRoot) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "z :: reverse(y::x::⟨⟩)")).head
    val yxNilResState = new ElaborateAction(HyperTermIdentifier(Identifier("a1")), yxNilPattern, yxNilRoot, maxSearchDepth = Some(12))(yxNilState.deepCopy())
    yxNilResState should not equal (yxNilState)

    // Now run casesplit and check all cases are matching
    val splitter = parser parseExpression "possibleSplit(l, ⟨⟩, x::⟨⟩, y::x::⟨⟩)"
    val splitterState = new ActionSearchState(Programs.empty
      .addTerm(parser parseExpression "a1 ||| reverse(l :+ z)")
      .addTerm(parser parseExpression "a2 ||| z :: reverse(l)")
      .addTerm(parser parseExpression "possibleSplit(l, ⟨⟩, x::⟨⟩, y::x::⟨⟩)"), rules)
    val conclusions = new CaseSplitAction(new OperatorRunAction(10), new OperatorRunAction(10)).getFoundConclusions(splitterState, 6)
    val correctId1 = splitterState.programs.queryGraph.findByEdgeType(HyperTermIdentifier(Identifier("a1"))).head.target
    val correctId2 = splitterState.programs.queryGraph.findByEdgeType(HyperTermIdentifier(Identifier("a2"))).head.target
    conclusions.find(_.contains(correctId1)).get should contain (correctId2)
  }

  test("test splitting filter p filter p l") {
    var state = new DefAction(parser apply "filter p (filter p (x :: y :: ⟨⟩)) = filter p (filter p l)")(new ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    val res = new ObservationalEquivalence(new CaseSplitAction(new OperatorRunAction(10), None, None, Some(3))).getEquives(state, 5)
//    val res = new CaseSplitAction(splitterChooser = None, splitDepthOption = Some(3), maxDepthOption = Some(8))
//      .getFoundConclusions(state)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (filter p (x :: y :: ⟨⟩))" cleanTypes)).head
    val patternResults = state.programs.queryGraph.findSubgraph[Int](pattern)
    patternResults should not be empty
    val correctId = patternResults.head.nodeMap(root.id)
    val correctSet = res._2.find(_.contains(correctId))
    correctSet should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (x :: y :: ⟨⟩)" cleanTypes)).head
    val correctId2 = state.programs.queryGraph.findSubgraph[Int](pattern2).head.nodeMap(root2.id)
    correctSet.get should contain (correctId2)
  }

  test("test splitting filter p filter q") {
    var state = new DefAction(parser apply "filter q (filter p (x :: y :: ⟨⟩)) = filter q (filter p l)")(new ActionSearchState(Programs.empty,
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    state = new DefAction(parser apply "filter p (filter q (x :: y :: ⟨⟩)) = filter p (filter q l)")(state)
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    val res = new CaseSplitAction(new OperatorRunAction(9), None, None, Some(4))
      .getFoundConclusions(state, 3, Some(2))
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter p (filter q (x :: y :: ⟨⟩))" cleanTypes)).head
    val patternResults = state.programs.queryGraph.findSubgraph[Int](pattern)
    patternResults should not be empty
    val correctId = patternResults.head.nodeMap(root.id)
    val correctSet = res.find(_.contains(correctId))
    correctSet should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(parser parseExpression "filter q (filter p (x :: y :: ⟨⟩))" cleanTypes)).head
    val correctId2 = state.programs.queryGraph.findSubgraph[Int](pattern2).head.nodeMap(root2.id)
    correctSet.get should contain (correctId2)
  }

  test("test can't split on same edge twice") {
    class Chooser() {
      var called: Int = 0
      val chooser = CaseSplitAction.randomChooser(2)
      def choose(state: ActionSearchState, chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]):
        Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
        val res = chooser(state, chosen)
        if (res.nonEmpty) {
          called += 1
        }
        res
      }
    }
    val chooser = new Chooser()
    val caseSplitAction = new CaseSplitAction(searcher, None, Some(chooser.choose), Some(2))
    val state = new ActionSearchState(Programs.empty.addTerm(parser parseExpression "possibleSplit(true, true, false)"),
      SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    caseSplitAction(state)
    chooser.called shouldEqual 1
  }
}