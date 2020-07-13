package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import structures.Hole
import synthesis._
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.Template.ReferenceTerm
import synthesis.search.actions.thesy.{Prover, SyGuERewriteRules, TheoryExplorationAction}
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, RewriteRule, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class TheoryExplorationActionTest extends FunSuite with Matchers with ParallelTestExecution with LazyLogging {
  val parser = new TranscalParser
  val predicate = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, Language.typeBoolean))
  val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
  val y = AnnotatedTree.identifierOnly(Identifier("y", Some(Language.typeInt)))
  val listInt = AnnotatedTree.withoutAnnotations(Language.typeListId, Seq(Language.typeInt))
  val listInttoListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, listInt))
  val inttoListIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, listInt, listInt))
  val listIntToIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, Language.typeInt, listInt))
  val listIntToListIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(listInt, listInt, listInt))
  val predicateToListIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(predicate, listInt, listInt))
  val typedCons = Language.consId.copy(annotation = Some(inttoListIntToListInt))
  val typedSnoc = Language.snocId.copy(annotation = Some(listIntToIntToListInt))
  val nil = AnnotatedTree.identifierOnly(Language.nilId.copy(annotation = Some(listInt)))
  val xnil = AnnotatedTree.withoutAnnotations(typedCons, Seq(x, nil))
  val xynil = AnnotatedTree.withoutAnnotations(typedCons, Seq(y, xnil))
  val reverse = AnnotatedTree.identifierOnly(Identifier("reverse", annotation = Some(listInttoListInt)))
  val filter = AnnotatedTree.identifierOnly(Identifier("filter", annotation = Some(predicateToListIntToListInt)))
  val concat = AnnotatedTree.identifierOnly(Identifier("concat", annotation = Some(listIntToListIntToListInt)))
  val tru = AnnotatedTree.identifierOnly(Language.trueId)
  val fals = AnnotatedTree.identifierOnly(Language.falseId)
  val spbeAction = new TheoryExplorationAction(Set(nil.root, typedCons), grammar = Set(reverse, filter), exampleDepth = 3)
  val listPh = thesy.inductionVar(listInt)
  val intPh = thesy.inductionVar(Language.typeInt)
  val predicatePh = thesy.inductionVar(predicate)

  // TODO: Split tests to conjecture generator tests and inductive step tests and integration tests.

  test("sygus step doesnt create terms deeper then needed") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons),
      grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc), concat),
      exampleDepth = 3,
      equivDepthOption = Some(0)
    )
    action.conjectureGenerator.increaseDepth()
    val actionState = action.conjectureGenerator.inferConjectures(Set.empty)
    logger.info(s"created base graph ${actionState.programs.queryGraph.size}")
    for (n <- SyGuERewriteRules.getSygusCreatedNodes(actionState.programs.queryGraph);
         t <- actionState.programs.reconstruct(n) if t.root != Language.typeId) {
      t.depth should be < 2
    }
    logger.info(s"finished reconstruct")

    action.conjectureGenerator.increaseDepth()
    val state1 = action.conjectureGenerator.inferConjectures(Set.empty)
    val relevantNodes2 = SyGuERewriteRules.getSygusCreatedNodes(state1.programs.queryGraph)
    logger.info(s"created depth 2 ${state1.programs.queryGraph.size}")
    for (n <- relevantNodes2;
         t <- state1.programs.reconstruct(n) if t.root != Language.typeId) {
      t.depth should be < 3
    }
    logger.info(s"finished reconstruct")

    //    val revRules = new LetAction(parser("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    //    val concatRules = new LetAction(parser("concat ?l1 ?l2 = l1 ++ l2")).rules
    //    val toMerge = action.findEquives(state1, revRules.toSeq ++ concatRules ++ SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    //
    //    for (targets <- toMerge if targets.size > 1;
    //         source = targets.head;
    //         target <- targets.tail) {
    //      state1.graph.mergeNodesInPlace(source, target)
    //    }
    //
    //    val state2 = action.sygusStep(state1)
    //    val progs2 = Programs(state2.graph)
    //    val relevantNodes3 = SyGuSRewriteRules.getSygusCreatedNodes(progs2.hyperGraph)
    //    logger.info(s"created depth 3 ${progs2.hyperGraph.size}")
    //    for (n <- relevantNodes3;
    //         t <- progs2.reconstruct(n) if t.root != Language.typeId) {
    //      t.depth should be < 4
    //    }
    //    logger.info(s"finished reconstruct")

  }

  test("testSygusStep can find reverse l") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse), exampleDepth = 3)
    action.conjectureGenerator.increaseDepth()
    val state = action.conjectureGenerator.inferConjectures(Set.empty)
    val pattern = Programs.destructPattern(new TranscalParser().parseExpression("(reverse: (list int) :> (list int)) _"))
    state.programs.queryGraph.findSubgraph[Int](pattern) should not be empty
  }

  test("testSygusStep can find reverse reverse l") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse), exampleDepth = 3)
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val state2 = action.conjectureGenerator.inferConjectures(Set.empty)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("(reverse: (list int) :> (list int)) _"))).head
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("reverse (reverse _)"))).head
    val results1 = state2.programs.queryGraph.findSubgraph[Int](pattern1)
    val results2 = state2.programs.queryGraph.findSubgraph[Int](pattern2)
    results1 should not be empty
    results2 should not be empty
    val results2Roots = results2.map(_.nodeMap(root2.id)).map(_.id)
    val results1Roots = results1.map(_.nodeMap(root1.id)).map(_.id)
    results1Roots.diff(results2Roots) should not be empty
  }

  test("test find that l == reverse reverse l") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse), exampleDepth = 3, equivDepthOption = Some(8))
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val state2 = action.conjectureGenerator.inferConjectures(Set.empty)
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val equives = action.conjectureGenerator.inferConjectures(AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    equives.programs.queryGraph should not be empty
    val programs = state2.programs
    val terms = equives.programs.queryGraph.nodes.map(id => programs.reconstruct(id))
    val correctSet = terms.map(_.toList).find(s => s.exists(t => t.copy(annotations = Seq.empty) == AnnotatedTree.identifierOnly(listPh.copy(annotation = None))))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    val reversePlaceholderTwice = correctSet.get.exists(t => t.root.literal == "reverse" && t.subtrees.head.root.literal == "reverse" && t.subtrees.head.subtrees.head.root == listPh)
    reversePlaceholderTwice shouldEqual true
  }

  test("testSygusStep can find filter p (filter p l)") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(filter), exampleDepth = 3)
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val state2 = action.conjectureGenerator.inferConjectures(Set.empty)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("filter ?p (filter ?p ?l)"))).head
    val results1 = state2.programs.queryGraph.findSubgraph[Int](pattern1)
    results1 should not be empty
  }

  test("test find that filter p (filter p l) == filter p l") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(filter), exampleDepth = 2, equivDepthOption = Some(6), splitDepthOption = Some(1))
    var aState = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(new ActionSearchState(Programs.empty, Set.empty[RewriteRule]))
    aState = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(aState)
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val state = action.conjectureGenerator.inferConjectures(Set.empty)
    val equives = action.conjectureGenerator.inferConjectures(AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ aState.rewriteRules)
    equives.programs.queryGraph should not be empty
    val programs = equives.programs
    val terms = equives.programs.queryGraph.nodes.map(id => programs.reconstruct(id).map(_.cleanTypes))
    val oneFilter = AnnotatedTree.withoutAnnotations(filter.root, Seq(
      AnnotatedTree.identifierOnly(predicatePh), AnnotatedTree.identifierOnly(listPh))) cleanTypes
    val correctSet = terms.map(_.toList.map(t => t.copy(annotations = Seq.empty) cleanTypes)).find(l =>
      l.contains(oneFilter))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    val filterOnce = correctSet.get.exists(t => t.root.literal == filter.root.literal
      && t.subtrees(1).root.copy(annotation = None) == listPh.copy(annotation = None))
    filterOnce shouldEqual true
  }

  test("Induction step for filter p (filter p l) == filter p l using case splitting") {
    var state = new ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules)
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    val predicateType = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, Language.typeBoolean))
    val typedFilter = AnnotatedTree.identifierOnly(Identifier("filter", Some(AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(predicateType, listInt, listInt)))))
    val spbeAction = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(typedFilter), exampleDepth = 2, equivDepthOption = Some(6), termDepthOption = Some(2), splitDepthOption = Some(1))
    val predicate = thesy.inductionVar(predicateType)
    val list = thesy.inductionVar(listInt)
    val filterOnPhs = AnnotatedTree.withoutAnnotations(typedFilter.root, Seq(predicate, list).map(AnnotatedTree.identifierOnly))
    val filterfilter = AnnotatedTree.withoutAnnotations(typedFilter.root, Seq(AnnotatedTree.identifierOnly(predicate), filterOnPhs))
    val prover = new Prover(spbeAction.vocab.datatypes.toSet, spbeAction.searcher, state.rewriteRules)
    prover.inductionProof(filterfilter, filterOnPhs) should not be empty
  }

  test("testSygusStep can find reverse(l :+ x) and (x :: reverse(l))") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc), AnnotatedTree.identifierOnly(typedCons)), exampleDepth = 3, equivDepthOption = Some(4))
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val state2 = action.conjectureGenerator.inferConjectures(Set.empty)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("reverse(_ :+ _)"))).head
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("_ :: (reverse _)"))).head
    val results1 = state2.programs.queryGraph.findSubgraph[Int](pattern1)
    val results2 = state2.programs.queryGraph.findSubgraph[Int](pattern2)
    results1 should not be empty
    results2 should not be empty
    val results2Roots = results2.map(_.nodeMap(root2.id)).map(_.id)
    val results1Roots = results1.map(_.nodeMap(root1.id)).map(_.id)
    results1Roots.diff(results2Roots) should not be empty
  }

  test("test find that reverse(l :+ x) == (x :: reverse(l))") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc), AnnotatedTree.identifierOnly(typedCons)), exampleDepth = 3, equivDepthOption = Some(4))
    action.conjectureGenerator.increaseDepth()
    action.conjectureGenerator.increaseDepth()
    val reverseRules = new LetAction(parser("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val state2 = action.conjectureGenerator.inferConjectures(AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.withoutAnnotations(reverse.root,
      Seq(AnnotatedTree.withoutAnnotations(Language.snocId, Seq(listPh.copy(annotation = None), intPh.copy(annotation = None)).map(AnnotatedTree.identifierOnly)))))).head
    state2.programs.queryGraph.findSubgraph[Int](pattern) should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.withoutAnnotations(Language.consId,
      Seq(AnnotatedTree.identifierOnly(intPh.copy(annotation = None)), AnnotatedTree.withoutAnnotations(reverse.root, Seq(AnnotatedTree.identifierOnly(listPh.copy(annotation = None)))))))).head
    val correctId = state2.programs.queryGraph.findSubgraph[Int](pattern).head.nodeMap(root.id)
    state2.programs.queryGraph.findSubgraph[Int](pattern2) should not be empty
    val correctId2 = state2.programs.queryGraph.findSubgraph[Int](pattern2).head.nodeMap(root2.id)
    //    state2.graph ++= state2.graph.nodes.map(n => ObservationalEquivalence.createAnchor(n))
    correctId shouldEqual correctId2
  }

  test("test induction steps proves reverse(l :+ x) == (x :: reverse(l))") {
    val action = new TheoryExplorationAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), exampleDepth = 3, equivDepthOption = Some(4))
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val state = new ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    val term1 = AnnotatedTree.withoutAnnotations(typedCons, List(
      AnnotatedTree.identifierOnly(intPh),
      AnnotatedTree.withoutAnnotations(reverse.root, List(AnnotatedTree.identifierOnly(listPh)))
    ))
    val term2 = AnnotatedTree.withoutAnnotations(reverse.root, List(
      AnnotatedTree.withoutAnnotations(typedSnoc, List(listPh, intPh).map(AnnotatedTree.identifierOnly))
    ))
    val prover = new Prover(spbeAction.vocab.datatypes.toSet, spbeAction.searcher, state.rewriteRules)
    val newRules = prover.inductionProof(term1, term2)
    newRules should not be empty
  }

  test("Cant proove x::xs == rev(xs) :+ x") {
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val state = new ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    val term1 = AnnotatedTree.withoutAnnotations(typedCons, List(
      intPh,
      listPh
    ).map(AnnotatedTree.identifierOnly))
    val term2 = AnnotatedTree.withoutAnnotations(typedSnoc, List(
      AnnotatedTree.withoutAnnotations(reverse.root, List(AnnotatedTree.identifierOnly(listPh))),
      AnnotatedTree.identifierOnly(intPh)
    ))
    val prover = new Prover(spbeAction.vocab.datatypes.toSet, spbeAction.searcher, state.rewriteRules)
    val newRules = prover.inductionProof(term1, term2)
    newRules shouldBe empty
  }

  test("fold can be found to equal sum") {
    val searcher = new OperatorRunAction(9)

    var state = new ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules)
    state = new LetAction(parser("sum ?l = l match ((⟨⟩ => 0) / ((?x :: ?xs) => x + sum(xs)))").cleanTypes)(state)
    state = new LetAction(parser("fold ?f ?i ?l = l match ((⟨⟩ => id i) / ((?x :: ?xs) => fold f (f(i,x)) xs))").cleanTypes)(state)
    state = new LetAction(parser("cons ?x ?l >> x :: l"))(state)
    state = new LetAction(parser("?x :: ?l >> cons ?x ?l").cleanTypes)(state)
    state = new LetAction(parser("pl ?x ?y >> x + y"))(state)
    state = new LetAction(parser("?x + ?y >> pl ?x ?y").cleanTypes)(state)
    state = new LetAction(parser("zero = 0"))(state)

    val equivs1 = new ObservationalEquivalence(searcher).fromTerms(
      Seq(
        parser.parseExpression("fold pl zero nil"),
        parser.parseExpression("sum nil")
      ),
      state.rewriteRules,
      6
    )
    equivs1.size shouldEqual 1

    val equivs2 = new ObservationalEquivalence(searcher).fromTerms(
      Seq(
        parser.parseExpression("fold pl zero (x::nil)"),
        parser.parseExpression("sum (x::nil)")
      ),
      state.rewriteRules,
      6
    )
    equivs2.size shouldEqual 1

    val equivs3 = new ObservationalEquivalence(searcher).fromTerms(
      Seq(
        parser.parseExpression("fold pl zero (y::x::nil)"),
        parser.parseExpression("sum (y::x::nil)")
      ),
      state.rewriteRules,
      6
    )
    equivs3.size shouldEqual 1

//    state = new DefAction(parser("fold pl zero nil = fold pl zero nil"))(state)
//    state = new DefAction(parser("sum nil = sum nil"))(state)
//    val typedPl = parser.parseExpression("pl : (int :> int :> int)")
//    val typedZero = parser.parseExpression("zero : int")
//    val typedFold = parser.parseExpression("fold : ((int :> int :> int) :> int :> (list int) :> int)")
//    val typedSum = parser.parseExpression("sum : ((list int) :> int)")
//    val action = new SPBEAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(typedFold, typedPl, typedSum, typedZero), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepthOption = Some(9), termDepthOption = Some(0), preRunDepth = Some(0))
//
//    val term1 = AnnotatedTree.withoutAnnotations(typedCons, List(
//      intPh,
//      listPh
//    ).map(AnnotatedTree.identifierOnly))
//    val term2 = AnnotatedTree.withoutAnnotations(typedSnoc, List(
//      AnnotatedTree.withoutAnnotations(reverse.root, List(AnnotatedTree.identifierOnly(listPh))),
//      AnnotatedTree.identifierOnly(intPh)
//    ))
//    val newRules = action.inductionStep(state, term1, term2)
//    newRules shouldBe empty
  }

  // This test exists in interperter tests
  //  test("Full SPBE run depth 2 reverse and snoc can proove reverse reverse l after run") {
  //    var state = ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules)
  //    state = new LetAction(parser("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))"))(state)
  //    state = new DefAction(parser("reverse(reverse(t)) = reverse(reverse(l))"))(state)
  //    val (aPattern, aRoot) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("l"))).head
  //    val lAnchor = HyperTermIdentifier(Identifier("a1"))
  //    state = new LocateAction(lAnchor, aPattern, Some(aRoot))(state)
  //    val (goal, root) = Programs.destructPatternsWithRoots(Seq(parser.parseExpression("reverse(reverse(l))"))).head
  //    val failedElaborateState = new ElaborateAction(lAnchor, goal, root, maxSearchDepth = Some(4))(state)
  //    failedElaborateState shouldEqual state
  //    val spbeAction = new SPBEAction(typeBuilders = Set(nil.root, typedCons), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepthOption = Some(6), termDepthOption = Some(2))
  //    state = spbeAction(state)
  //    val successfulState = new ElaborateAction(lAnchor, goal, root, maxSearchDepth = Some(4))(state)
  //    successfulState should not be (state)
  //    val lTarget = successfulState.programs.hyperGraph.findSubgraph[Int](goal).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
  //    val anchorTarget = successfulState.programs.hyperGraph.findByEdgeType(lAnchor).head.target
  //    lTarget shouldEqual anchorTarget
  //  }
}
