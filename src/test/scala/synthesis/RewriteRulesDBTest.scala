package synthesis

import org.scalatest.{FunSuite, Matchers}
import structures.HyperEdge
import synthesis.Programs.NonConstructableMetadata
import synthesis.complexity.{AddComplexity, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Identifier, TranscalParser}

class RewriteRulesDBTest extends FunSuite with Matchers {

  test("rewriteRules manage to rewrite a + b + c to (a + b) + c") {
    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = AssociativeRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.findSubgraph[Int](pattern).nonEmpty) shouldEqual true
  }

  //  property("rewriteRules manage to rewrite ?x / false >> id x") {
  //    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
  //    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
  //    val pattern = Programs.destructPattern(patternTerm, Set.empty)
  //    val state = new RewriteSearchState(Programs.destruct(term))
  //    val rules = AssociativeRewriteRulesDB.rewriteRules
  //    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  //  }
  //
  //  property("rewriteRules doesnt rewrite false in ?x / false >> id x") {
  //    val term = new TranscalParser().apply("1 -> a + (b + c)").subtrees(1)
  //    val patternTerm = new TranscalParser().apply("1 -> ((_ + _) + _)").subtrees(1)
  //    val pattern = Programs.destructPattern(patternTerm, Set.empty)
  //    val state = new RewriteSearchState(Programs.destruct(term))
  //    val rules = AssociativeRewriteRulesDB.rewriteRules
  //    check(rules.exists(_.apply(state).graph.findSubgraph(pattern).nonEmpty))
  //  }

  test("rewriteRules manage to rewrite (?x le ?y) ||> min(x, y) >> id x") {
    val term = new TranscalParser().parseExpression("min(a, b) |||| ((a <= b) ||| true)")
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(r => {
      val newProgs = synthesis.Programs(r(state).graph)
      val aRoot = newProgs.hyperGraph.findEdges(HyperTermIdentifier(Identifier("a"))).head.target
      newProgs.reconstruct(aRoot).contains(new TranscalParser().parseExpression("min(a, b)"))
    }) shouldEqual true
  }

  test("rewriteRules doesnt rewrite (?x le ?y) ||> min(x, y) >> id(x) by mistake") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val patternTerm = new TranscalParser().apply("1 -> id a").subtrees(1)
    val pattern = Programs.destructPattern(patternTerm)
    val state = new RewriteSearchState(Programs.destruct(term))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(_.apply(state).graph.findSubgraph[Int](pattern).isEmpty) shouldEqual true
  }

  test("rule to a single hole works") {
    val term = new TranscalParser().parseExpression("l ++ ⟨⟩")
    val patternTerm = new TranscalParser().parseExpression("l")
    val (pattern, patternRoot) = Programs.destructPatternsWithRoots(Seq(patternTerm)).head
    val (graph, root) = Programs.destructWithRoot(term)
    val anchor = HyperTermIdentifier(Identifier("anchor"))
    val state = new RewriteSearchState(graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata))
    val rules = SimpleRewriteRulesDB.rewriteRules
    rules.exists(r => {
      val newState = r.apply(state)
      val id = patternRoot.asInstanceOf[ReferenceTerm[HyperTermId]].id
      newState.graph.findSubgraph[Int](pattern).head._1(id) == newState.graph.findEdges(anchor).head.target
    }) shouldEqual true
  }

  test("And associativity") {
    val rules = AssociativeRewriteRulesDB.rewriteRules
    def validate(term: AnnotatedTree, patternTerm: AnnotatedTree) = {
      val (pattern, patternRoot) = Programs.destructPatternsWithRoots(Seq(patternTerm)).head
      val (graph, root) = Programs.destructWithRoot(term)
      val anchor = HyperTermIdentifier(Identifier("anchor"))
      val state = new RewriteSearchState(graph + HyperEdge(root, anchor, Seq.empty, NonConstructableMetadata))
      rules.exists(r => {
        val newState = r.apply(state)
        val id = patternRoot.asInstanceOf[ReferenceTerm[HyperTermId]].id
        val findRes = newState.graph.findSubgraph[Int](pattern)
        findRes.nonEmpty && findRes.head._1(id) == newState.graph.findEdges(anchor).head.target
      }) shouldEqual true
    }

    val term1 = new TranscalParser().parseExpression("((x ∧ y) ∧ z)")
    val term2 = new TranscalParser().parseExpression("(x ∧ (y ∧ z))")
    validate(term1, term2)
    validate(term2, term1)
  }

  test("Reconstruct simple value") {
    val parser = new TranscalParser
    val tree = parser.apply("timecomplexTrue = timecomplex x 0")
    val programs = Programs.empty.addTerm(tree)

    val headEdge = programs.hyperGraph.find(e => e.sources.isEmpty && e.edgeType.identifier == Identifier("x")).map(_.target)
    assume(headEdge.nonEmpty)
    val result = programs.reconstructWithTimeComplex(headEdge.get).toSeq
    result == Seq((AnnotatedTree.identifierOnly(Identifier("x")), ConstantComplexity(0)))
  }

  test("Reconstruct concat time complex") {
    val parser = new TranscalParser
    val tree1 = parser.parseExpression("timecomplex x 0 = timecomplexTrue")
    val tree2 = parser.parseExpression("timecomplex xs 0 = timecomplexTrue")
    val tree3 = parser.parseExpression("x :: xs")

    val graphBefore = {
      val programs = Programs.empty + tree1 + tree2 + tree3
      programs.hyperGraph
    }
    val graphAfter = TimeComplexRewriteRulesDB.rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphBefore.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
    assume((graphAfter -- graphBefore).nonEmpty)

    val programs = Programs(graphAfter)

    val xEdgeOption = programs.hyperGraph.find(e => e.sources.isEmpty && e.edgeType.identifier == Identifier("x")).map(_.target)
    assume(xEdgeOption.nonEmpty)
    val xsEdgeOption = programs.hyperGraph.find(e => e.sources.isEmpty && e.edgeType.identifier == Identifier("xs")).map(_.target)
    assume(xsEdgeOption.nonEmpty)

    val concatEdgeOption = programs.hyperGraph.find(e => e.sources == Seq(xEdgeOption.get, xsEdgeOption.get) && e.edgeType.identifier == Identifier("::")).map(_.target)
    assume(concatEdgeOption.nonEmpty)

    val result = programs.reconstructWithTimeComplex(concatEdgeOption.get).toSeq
    val expectedTree = tree3
    val expectedComplexity = AddComplexity(Seq(ConstantComplexity(1)))
    programs.reconstructWithTimeComplex(xEdgeOption.get).toSeq == Seq((AnnotatedTree.identifierOnly(Identifier("x")), ConstantComplexity(0))) &&
      programs.reconstructWithTimeComplex(xsEdgeOption.get).toSeq == Seq((AnnotatedTree.identifierOnly(Identifier("xs")), ConstantComplexity(0))) &&
      (result == Seq((expectedTree, expectedComplexity)))
  }

  test("Reconstruct elems time complex") {
    val parser = new TranscalParser
    val tree1 = parser.parseExpression("timecomplex xs 0 = timecomplexTrue")
    val tree2 = parser.parseExpression("spacecomplex xs (len(xs)) = spacecomplexTrue")
    val tree3 = parser.parseExpression("elems(xs)")

    val programs = {
      val graphBefore = {
        val programs = Programs.empty + tree1 + tree2 + tree3
        programs.hyperGraph
      }
      val graphAfter1 = SpaceComplexRewriteRulesDB.rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphBefore.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter1 -- graphBefore).nonEmpty)

      val graphAfter2 = TimeComplexRewriteRulesDB.rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphAfter1.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter2 -- graphAfter1).nonEmpty)

      Programs(graphAfter2)
    }

    val elemsEdgeOption = programs.hyperGraph.find(e => e.sources.size == 1 && e.edgeType.identifier == Identifier("elems")).map(_.target)
    assume(elemsEdgeOption.nonEmpty)

    val result = programs.reconstructWithTimeComplex(elemsEdgeOption.get).toSeq
    val expectedTree = tree3
    val expectedComplexity = AddComplexity(Seq(ConstantComplexity(1), ContainerComplexity("len(xs)")))
    result == Seq((expectedTree, expectedComplexity))
  }

  test("Reconstruct ∪ time complex") {
    val parser = new TranscalParser
    val tree1 = parser.parseExpression("timecomplex x 0 = timecomplexTrue")
    val tree2 = parser.parseExpression("timecomplex xs (len(xs)) = timecomplexTrue")
    val tree3 = parser.parseExpression("spacecomplex xs (len(xs)) = spacecomplexTrue")
    val tree4 = parser.parseExpression("elems(x :: xs)")

    val programs = {
      val graphBefore = {
        val programs = Programs.empty + tree1 + tree2 + tree3 + tree4
        programs.hyperGraph
      }
      val rewriteRules = SimpleRewriteRulesDB.rewriteRules.toSeq ++ TimeComplexRewriteRulesDB.rewriteRules ++ SpaceComplexRewriteRulesDB.rewriteRules
      val graphAfter1 = rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphBefore.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter1 -- graphBefore).nonEmpty)

      val graphAfter2 = rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphAfter1.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter2 -- graphAfter1).nonEmpty)

      Programs(graphAfter2)
    }

    val elemsEdgeOption = programs.hyperGraph.find(e => e.sources.size == 2 && e.edgeType.identifier == Identifier("∪")).map(_.target)
    assume(elemsEdgeOption.nonEmpty)

    val result = programs.reconstructWithTimeComplex(elemsEdgeOption.get).toSet
    val expectedTree = parser.parseExpression("{x} ∪ (elems xs)")
    val expectedComplexity = AddComplexity(Seq(ConstantComplexity(4), ContainerComplexity("len(xs)"), ContainerComplexity("len(xs)"), ContainerComplexity("len(xs)")))
    1 == result.size && (expectedTree, expectedComplexity) == result.head
  }

  test("Reconstruct ‖ time complex") {
    val parser = new TranscalParser
    val tree1 = parser.parseExpression("timecomplex x 0 = timecomplexTrue")
    val tree2 = parser.parseExpression("timecomplex xs (len(x)) = timecomplexTrue")
    val tree3 = parser.parseExpression("spacecomplex xs (len(xs)) = spacecomplexTrue")
    val tree4 = parser.parseExpression("{x} ‖ elems(xs)")

    val programs = {
      val graphBefore = {
        val programs = Programs.empty + tree1 + tree2 + tree3 + tree4
        programs.hyperGraph
      }
      val rewriteRules = TimeComplexRewriteRulesDB.rewriteRules ++ SpaceComplexRewriteRulesDB.rewriteRules
      val graphAfter1 = rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphBefore.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter1 -- graphBefore).nonEmpty)

      val graphAfter2 = rewriteRules.foldLeft(structures.mutable.VersionedHyperGraph(graphAfter1.toSeq:_*))((g, o) => o.apply(RewriteSearchState(g)).graph)
      assume((graphAfter2 -- graphAfter1).nonEmpty)

      Programs(graphAfter2)
    }

    val elemsEdgeOption = programs.hyperGraph.find(e => e.sources.size == 2 && e.edgeType.identifier == Identifier("‖")).map(_.target)
    assume(elemsEdgeOption.nonEmpty)

    val result = programs.reconstructWithTimeComplex(elemsEdgeOption.get).toSeq
    val expectedTree = tree4
    val expectedComplexity = AddComplexity(Seq(ConstantComplexity(1), ContainerComplexity("len(xs)")))
    result == Seq((expectedTree, expectedComplexity))
  }
}
