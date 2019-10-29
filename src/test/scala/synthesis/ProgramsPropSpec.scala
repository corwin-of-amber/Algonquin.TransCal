package synthesis

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.immutable.CompactHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.rewrites.Template.ReferenceTerm
import transcallang.Language._
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

class ProgramsPropSpec extends PropSpec with Checkers with ParallelTestExecution  {

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

  property("10 first nodes are constructable") {
    check(forAll { programs: Programs =>
      programs.hyperGraph.nodes.take(10).map(programs.reconstruct).forall(_.nonEmpty)
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
        val edges = progs.hyperGraph.findEdges(HyperTermIdentifier(Language.idId))
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
    val graph = CompactHyperGraph(
        HyperEdge(HyperTermId(1), HyperTermIdentifier(Identifier("x")), List(), EmptyMetadata),
        HyperEdge(HyperTermId(2), HyperTermIdentifier(Identifier("xs")), List(), EmptyMetadata),
        HyperEdge(HyperTermId(7), HyperTermIdentifier(Identifier("elem")), List(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(7), HyperTermIdentifier(Language.setDisjointId), List(HyperTermId(15), HyperTermId(12)), EmptyMetadata),
        HyperEdge(HyperTermId(8), HyperTermIdentifier(Language.setNotContainsId), List(HyperTermId(1), HyperTermId(12)), EmptyMetadata),
        HyperEdge(HyperTermId(8), HyperTermIdentifier(Identifier("¬")), List(HyperTermId(7)), EmptyMetadata),
        HyperEdge(HyperTermId(10), HyperTermIdentifier(Identifier("nodup")), List(HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(11), HyperTermIdentifier(Language.andId), List(HyperTermId(8), HyperTermId(10)), EmptyMetadata),
        HyperEdge(HyperTermId(12), HyperTermIdentifier(Identifier("elems")), List(HyperTermId(2)), EmptyMetadata),
        HyperEdge(HyperTermId(15), HyperTermIdentifier(Language.setId), List(HyperTermId(1)), EmptyMetadata)
      )
    val terms = new Programs(graph).reconstruct(HyperTermId(11))
    check(terms.exists((t: AnnotatedTree) => t.nodes.map(_.root).contains(Language.setDisjointId)))
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

  property("Destruct typed list with max hypertermid with correct root") {
    val listInt = AnnotatedTree.withoutAnnotations(Language.typeListId, Seq(Language.typeInt))
    val inttoListIntToListInt = AnnotatedTree.withoutAnnotations(Language.mapTypeId, Seq(Language.typeInt, listInt, listInt))
    val x = AnnotatedTree.identifierOnly(Identifier("x", Some(Language.typeInt)))
    val y = AnnotatedTree.identifierOnly(Identifier("y", Some(Language.typeInt)))
    val typedCons = Language.consId.copy(annotation = Some(inttoListIntToListInt))
    val nil = AnnotatedTree.identifierOnly(Language.nilId.copy(annotation = Some(listInt)))
    val xnil = AnnotatedTree.withoutAnnotations(typedCons, Seq(x, nil))
    val xynil = AnnotatedTree.withoutAnnotations(typedCons, Seq(y, xnil))

    val (tempGraph, root) = Programs.destructWithRoot(xynil,
      maxId = HyperTermId(558))

    check(tempGraph.map(_.target).contains(root))
  }

  property("Destruct ?x >> reverse(reverse(?x)) is correct regarding target and source") {
    val tree = new TranscalParser()("?x >> reverse(reverse(?x))")
    val res = Programs.destructPatternsWithRoots(tree.subtrees)
    check(res.head._2 == res.last._2)
    val rootEdge = res.last._1.findByTarget[Int](res.last._2).head
    val innerEdge = res.last._1.findByTarget[Int](rootEdge.sources.head).head
    check(res.last._2 == innerEdge.sources.head)
  }

  property("a few reconstruct in a row returns same results (using mutable state now)") {
    check(forAll { programs: Programs => programs.hyperGraph.nonEmpty ==> {
      (0 to 3).map(_ => programs.reconstruct(programs.hyperGraph.nodes.head).toSet).toSet.size == 1
    }})
  }
}
