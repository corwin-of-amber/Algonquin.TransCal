package synthesis

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.Checkers
import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.complexity.{AddComplexity, ConstantComplexity, ContainerComplexity}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.Language._
import transcallang.{AnnotatedTree, Identifier, TranscalParser}

class ProgramsPropSpec extends PropSpec with Checkers {

  private implicit val identifierCreator: Arbitrary[Identifier] = Arbitrary(identifierGen)
  private implicit val termsCreator: Arbitrary[AnnotatedTree] = Arbitrary(identifierTreesGen)
  private implicit val programsCreator: Arbitrary[Programs] = Arbitrary(programsGen)

  property("main program is in reconstruct") {
    check(forAll { term: AnnotatedTree => {
      val (graph, root) = Programs.destructWithRoot(term)
      val programs = Programs(graph)
      programs.reconstruct(root).contains(term) :| programs.toString
    }
    })
  }

  property("every node is constructable") {
    check(forAll { programs: Programs =>
      programs.hyperGraph.nodes.map(programs.reconstruct).forall(_.nonEmpty)
    })
  }

  property("be able to handle a tree of one and return it") {
    check(forAll { (root: Identifier, son: Identifier) => {
      val tree = new AnnotatedTree(root, List(AnnotatedTree.identifierOnly(son)), Seq.empty)
      val programs = Programs(tree)

      val results = programs.reconstruct(HyperTermId(programs.hyperGraph.nodes.map { case HyperTermId(i) => i }.max)).toList

      results.size == 1 && results.contains(tree)
    }
    })
  }

  property("be able to handle a tree with 1 depth and return it") {
    check(forAll { (root: Identifier, son1: Identifier, son2: Identifier) =>
      (root != son1 && root != son2) ==> {
        val tree = new AnnotatedTree(root, List(AnnotatedTree.identifierOnly(son1), AnnotatedTree.identifierOnly(son2)), Seq.empty)
        val programs = Programs(tree)

        val results = {
          val mainHyperTermId = programs.hyperGraph.findEdges(HyperTermIdentifier(root))
          programs.reconstruct(mainHyperTermId.head.target).toList
        }

        results.size == 1 && results.contains(tree)
      }
    })
  }

  property("unknown term returns empty iterator") {
    check(forAll { programs: Programs =>
      programs.reconstruct(HyperTermId(-1)).isEmpty
    })
  }

  property("destruct splitted term and find using pattern") {
    check(forAll { (term1: AnnotatedTree, term2: AnnotatedTree) =>
      (term1.nodes ++ term2.nodes).map(_.root).intersect(Seq("/", "id")).isEmpty ==> {
        val progs = Programs(new AnnotatedTree(splitId, List(term1, term2), Seq.empty))
        val edges = progs.hyperGraph.findEdges(HyperTermIdentifier(Identifier("id")))
        edges.map(_.target).forall(t => progs.reconstruct(t).toSeq.intersect(Seq(term1, term2)).nonEmpty)
      }
    })
  }

  property("destruct pattern has right amount of references") {
    val parser = new TranscalParser
    val pattern1 = Programs.destructPattern(parser("_ -> _ + _").subtrees(1))
    check(pattern1.nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 3)
    val pattern2 = Programs.destructPattern(parser("_ -> _ + _ - _").subtrees(1))
    check(pattern2.nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 5)
    val pattern3 = Programs.destructPattern(parser("?x ?y -> _ + x + y").subtrees(1))
    check(pattern3.nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 5)
  }

  property("destruct apply and reconstruct should work correctly") {
    val parser = new TranscalParser
    val term = parser("_ -> (a b) c d")
    val graph = Programs.destruct(term)
    check(graph.edgeTypes.count(_.identifier.literal == "a") == 1)
    check(graph.edgeTypes.count(_.identifier.literal == "b") == 1)
    check(graph.edges.filter(_.edgeType.identifier.literal == "a").head.sources.size == 3)
  }

  property("Reconstructs more then one possibility") {
    val graph = VersionedHyperGraph(
      Seq(
        HyperEdge(HyperTermId(1), HyperTermIdentifier(Identifier("x")), List(), EmptyMetadata),
        HyperEdge(HyperTermId(2), HyperTermIdentifier(Identifier("xs")), List(), EmptyMetadata),
        HyperEdge(HyperTermId(7), HyperTermIdentifier(Identifier("elem")), List(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(7), HyperTermIdentifier(Identifier("‖")), List(HyperTermId(15), HyperTermId(12)), EmptyMetadata),
        HyperEdge(HyperTermId(8), HyperTermIdentifier(Identifier("∉")), List(HyperTermId(1), HyperTermId(12)), EmptyMetadata),
        HyperEdge(HyperTermId(8), HyperTermIdentifier(Identifier("¬")), List(HyperTermId(7)), EmptyMetadata),
        HyperEdge(HyperTermId(10), HyperTermIdentifier(Identifier("nodup")), List(HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(11), HyperTermIdentifier(Identifier("∧")), List(HyperTermId(8), HyperTermId(10)), EmptyMetadata),
        HyperEdge(HyperTermId(12), HyperTermIdentifier(Identifier("elems")), List(HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(15), HyperTermIdentifier(Identifier("{.}")), List(HyperTermId(1)), EmptyMetadata)
      ): _*)
    val terms = new Programs(graph).reconstruct(HyperTermId(11))
    check(terms.exists((t: AnnotatedTree) => t.nodes.map(_.root.literal).contains("‖")))
  }

  property("when deconstructing any hole create a special edge to match all nodes") {
    val parser = new TranscalParser
    val term = parser("?x -> x")
    val graphs = Programs.destructPatterns(Seq(term.subtrees(0), term.subtrees(1)))
    check(graphs.forall(_.edgeTypes.exists(_.isInstanceOf[ReferenceTerm[HyperTermIdentifier]])))
  }

  property("when deconstructing orcondbuilder get a graph with 2 roots") {
    val parser = new TranscalParser
    val pattern = parser.parseExpression("x |||| int")
    val pattern2 = parser.parseExpression("x ||| int")
    val graph = Programs.destructPattern(pattern)
    val graph2 = Programs.destructPattern(pattern2)
    check(graph.nodes.size > 1)
    check(graph.nodes.size > graph2.nodes.size)
  }

  property("Reconstruct simple value") {
    val parser = new TranscalParser
    val tree = parser.apply("timecomplexTrue = timecomplex x 0")
    val programs = Programs.empty.addTerm(tree)

    val headEdge = programs.hyperGraph.find(e => e.sources.isEmpty && e.edgeType.identifier == Identifier("x")).map(_.target)
    assume(headEdge.nonEmpty)
    val result = programs.reconstructWithTimeComplex(headEdge.get).toSeq
    check(result == Seq((AnnotatedTree.identifierOnly(Identifier("x")), ConstantComplexity(0))))
  }

  property("Reconstruct concat time complex") {
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
    check(programs.reconstructWithTimeComplex(xEdgeOption.get).toSeq == Seq((AnnotatedTree.identifierOnly(Identifier("x")), ConstantComplexity(0))))
    val xsEdgeOption = programs.hyperGraph.find(e => e.sources.isEmpty && e.edgeType.identifier == Identifier("xs")).map(_.target)
    assume(xsEdgeOption.nonEmpty)
    check(programs.reconstructWithTimeComplex(xsEdgeOption.get).toSeq == Seq((AnnotatedTree.identifierOnly(Identifier("xs")), ConstantComplexity(0))))

    val concatEdgeOption = programs.hyperGraph.find(e => e.sources == Seq(xEdgeOption.get, xsEdgeOption.get) && e.edgeType.identifier == Identifier("::")).map(_.target)
    assume(concatEdgeOption.nonEmpty)

    val result = programs.reconstructWithTimeComplex(concatEdgeOption.get).toSeq
    val expectedTree = tree3
    val expectedComplexity = AddComplexity(Seq(ConstantComplexity(1)))
    check(result == Seq((expectedTree, expectedComplexity)))
  }

  property("Reconstruct elems time complex") {
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
    check(result == Seq((expectedTree, expectedComplexity)))
  }
}
