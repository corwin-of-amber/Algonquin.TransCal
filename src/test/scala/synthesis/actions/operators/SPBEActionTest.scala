package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import structures.Hole
import synthesis._
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class SPBEActionTest extends FunSuite with Matchers with ParallelTestExecution with LazyLogging {
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
  val concat =  AnnotatedTree.identifierOnly(Identifier("concat", annotation = Some(listIntToListIntToListInt)))
  val tru = AnnotatedTree.identifierOnly(Language.trueId)
  val fals = AnnotatedTree.identifierOnly(Language.falseId)
  val listPh = AnnotatedTree.identifierOnly(Identifier("Placeholder_0_type_{list(int)}", annotation = Some(listInt)))
  val intPh = AnnotatedTree.identifierOnly(Identifier("Placeholder_0_type_{int}", annotation = Some(Language.typeInt)))

  test("sygus step doesnt create terms deeper then needed") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)),
      grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc), concat),
      examples = Map(listInt -> Seq(nil, xnil, xynil)),
      equivDepth = 6
    )
    val baseProgs = Programs(action.baseGraph)
    val relevantNodes = SyGuSRewriteRules.getSygusCreatedNodes(baseProgs.hyperGraph)
    logger.info(s"created base graph ${baseProgs.hyperGraph.size}")
    for (n <- relevantNodes;
         t <- baseProgs.reconstruct(n) if t.root != Language.typeId) {
      t.depth should be < 2
    }
    logger.info(s"finished reconstruct")

    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val progs = Programs(state1.graph)
    val relevantNodes2 = SyGuSRewriteRules.getSygusCreatedNodes(progs.hyperGraph)
    logger.info(s"created depth 2 ${progs.hyperGraph.size}")
    for (n <- relevantNodes2;
         t <- progs.reconstruct(n) if t.root != Language.typeId) {
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
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse), examples = Map(listInt -> Seq(nil, xnil, xynil)))
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
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 8)
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val equives = action.findEquives(state2, AssociativeRewriteRulesDB.rewriteRules.toSeq ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    equives should not be empty
    equives.forall({ s => s.forall(state2.graph.nodes.contains) }) should be(true)
    val programs = Programs(state2.graph)
    val terms = equives.map(s => s.map(id => programs.reconstruct(id)))
    val correctSet = terms.map(_.map(_.toList)).find(s => s.exists(l => l.exists(t => t.copy(annotations = Seq.empty) == listPh)))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    val reversePlaceholderTwice = correctSet.get.exists(_.exists(t => t.root.literal == "reverse" && t.subtrees.head.root.literal == "reverse" && t.subtrees.head.subtrees.head.root == listPh.root))
    reversePlaceholderTwice shouldEqual true
  }

  test("testSygusStep can find filter p (filter p l)") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(filter), examples = Map(listInt -> Seq(nil, xnil, xynil)))
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("filter ?p (filter ?p ?l)"))).head
    val results1 = state2.graph.findSubgraph[Int](pattern1)
    results1 should not be empty
  }

  test("test find that filter p (filter p l) == filter p l") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(filter), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 8, splitDepth = 2)
    var aState = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(ActionSearchState(Programs.empty, Set.empty))
    aState = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(aState)
    var state = action.sygusStep(new RewriteSearchState(action.baseGraph))
    state = action.sygusStep(state)
    state.graph ++= state.graph.nodes.map(n => ObservationalEquivalence.createAnchor(n))
    val equives = action.findEquives(state, AssociativeRewriteRulesDB.rewriteRules.toSeq ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ aState.rewriteRules)
    equives should not be empty
    equives.forall({ s => s.forall(state.graph.nodes.contains) }) should be(true)
    val programs = Programs(state.graph)
    val terms = equives.map(s => s.map(id => programs.reconstruct(id)))
    val correctSet = terms.map(_.map(_.toList)).find(s =>
      s.exists(l =>
        l.exists(t =>
          t.root.literal == filter.root.literal
            && t.subtrees.size == 2
            && t.subtrees(1).root.literal == filter.root.literal
            && t.subtrees(1).subtrees.size == 2
            && t.subtrees(1).subtrees(1).root.copy(annotation = None) == listPh.root.copy(annotation = None))))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    val filterOnce = correctSet.get.exists(_.exists(t => t.root.literal == filter.root.literal
      && t.subtrees(1).root.copy(annotation = None) == listPh.root.copy(annotation = None)))
    filterOnce shouldEqual true
  }

  test("Induction step for filter p (filter p l) == filter p l using case splitting") {
    var state = ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules)
    state = new LetAction(parser("filter ?p ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (p x) match ((true =>  x :: (filter p xs)) / (false => filter p xs))))"))(state)
    state = new LetAction(parser(s"filter ?p (?x::?xs) |>> ${CaseSplitAction.splitTrue.literal} ||| ${CaseSplitAction.possibleSplitId.literal}((p x), true, false)"))(state)
    val predicateType = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, Language.typeBoolean))
    val typedFilter = AnnotatedTree.identifierOnly(Identifier("filter", Some(AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(predicateType, listInt, listInt)))))
    val spbeAction = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(typedFilter), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 8, termDepth = 2, splitDepth = 3)
    val predicate = spbeAction.createPlaceholder(predicateType, 0)
    val list = spbeAction.createPlaceholder(listInt, 0)
    val filterOnPhs = AnnotatedTree.withoutAnnotations(typedFilter.root, Seq(predicate, list).map(AnnotatedTree.identifierOnly))
    val filterfilter = AnnotatedTree.withoutAnnotations(typedFilter.root, Seq(AnnotatedTree.identifierOnly(predicate), filterOnPhs))
    spbeAction.inductionStep(state, filterfilter, filterOnPhs) should not be empty
  }

  test("testSygusStep can find reverse(l :+ x) and (x :: reverse(l))") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 6)
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val (pattern1, root1) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("reverse(_ :+ _)"))).head
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(new TranscalParser().parseExpression("_ :: (reverse _)"))).head
    val results1 = state2.graph.findSubgraph[Int](pattern1)
    val results2 = state2.graph.findSubgraph[Int](pattern2)
    results1 should not be empty
    results2 should not be empty
    val results2Roots = results2.map(_._1(root2.asInstanceOf[ReferenceTerm[HyperTermId]].id)).map(_.id)
    val results1Roots = results1.map(_._1(root1.asInstanceOf[ReferenceTerm[HyperTermId]].id)).map(_.id)
    results1Roots.diff(results2Roots) should not be empty
  }

  test("test find that reverse(l :+ x) == (x :: reverse(l))") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 4)
    val state1 = action.sygusStep(new RewriteSearchState(action.baseGraph))
    val state2 = action.sygusStep(state1)
    val reverseRules = new LetAction(parser("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val (pattern, root) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.withoutAnnotations(reverse.root,
      Seq(AnnotatedTree.withoutAnnotations(Language.snocId, Seq(listPh.map(_.copy(annotation = None)), intPh.map(_.copy(annotation = None)))))))).head
    state2.graph.findSubgraph[Int](pattern) should not be empty
    val (pattern2, root2) = Programs.destructPatternsWithRoots(Seq(AnnotatedTree.withoutAnnotations(Language.consId,
      Seq(intPh.map(_.copy(annotation = None)), AnnotatedTree.withoutAnnotations(reverse.root, Seq(listPh.map(_.copy(annotation = None)))))))).head
    val correctId = state2.graph.findSubgraph[Int](pattern).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    state2.graph.findSubgraph[Int](pattern2) should not be empty
    val correctId2 = state2.graph.findSubgraph[Int](pattern2).head._1(root2.asInstanceOf[ReferenceTerm[HyperTermId]].id)
//    state2.graph ++= state2.graph.nodes.map(n => ObservationalEquivalence.createAnchor(n))
    val nodes = state2.graph.nodes
    val equives = action.findEquives(state2, AssociativeRewriteRulesDB.rewriteRules.toSeq ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    state2.graph.nodes shouldEqual nodes
    equives should not be empty
    equives.forall({ s => s.forall(state2.graph.nodes.contains) }) should be(true)
    val programs = Programs(state2.graph)
//    val terms = equives.map(s => s.map(id => programs.reconstruct(id).toSeq))
    val correctSet = equives.find(_.contains(correctId))
    correctSet should not be empty
    println("Found correct set of equives")
    print(correctSet.get)
    correctSet.get should contain (correctId2)
  }

  test("test induction steps proves reverse(l :+ x) == (x :: reverse(l))") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 6)
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val state = new ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    val term1 = AnnotatedTree.withoutAnnotations(typedCons, List(
      intPh,
      AnnotatedTree.withoutAnnotations(reverse.root, List(listPh))
    ))
    val term2 = AnnotatedTree.withoutAnnotations(reverse.root, List(
      AnnotatedTree.withoutAnnotations(typedSnoc, List(listPh, intPh))
    ))
    val newRules = action.inductionStep(state, term1, term2)
    newRules should not be empty
  }

  test("Cant proove x::xs == rev(xs) :+ x") {
    val action = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 6, termDepth = 3)
    val reverseRules = new LetAction(new TranscalParser()("reverse ?l = l match ((⟨⟩ => ⟨⟩) / ((?x :: ?xs) => (reverse xs) :+ x))")).rules
    val state = ActionSearchState(Programs.empty, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules ++ SystemRewriteRulesDB.rewriteRules ++ reverseRules)
    val term1 = AnnotatedTree.withoutAnnotations(typedCons, List(
      intPh,
      listPh
    ))
    val term2 = AnnotatedTree.withoutAnnotations(typedSnoc, List(
      AnnotatedTree.withoutAnnotations(reverse.root, List(listPh)),
      intPh
    ))
    val newRules = action.inductionStep(state, term1, term2)
    newRules shouldBe empty
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
  //    val spbeAction = new SPBEAction(typeBuilders = Set(nil, AnnotatedTree.identifierOnly(typedCons)), grammar = Set(reverse, AnnotatedTree.identifierOnly(typedSnoc)), examples = Map(listInt -> Seq(nil, xnil, xynil)), equivDepth = 6, termDepth = 2)
  //    state = spbeAction(state)
  //    val successfulState = new ElaborateAction(lAnchor, goal, root, maxSearchDepth = Some(4))(state)
  //    successfulState should not be (state)
  //    val lTarget = successfulState.programs.hyperGraph.findSubgraph[Int](goal).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
  //    val anchorTarget = successfulState.programs.hyperGraph.findByEdgeType(lAnchor).head.target
  //    lTarget shouldEqual anchorTarget
  //  }
}
