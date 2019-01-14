package synthesis

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.{EmptyMetadata, HyperEdge}
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import synthesis.language.TranscalParser
import synthesis.rewrites.Template.ReferenceTerm

class ProgramsPropSpec extends PropSpec with Checkers {

  implicit val identifierCreator = Arbitrary(identifierGen)
  implicit val termsCreator = Arbitrary(identifierTreesGen)
  implicit val programsCreator = Arbitrary(programsGen)

  property("main program is in reconstruct") {
    check(forAll { term: Term => {
      val programs = Programs(term)
      programs.reconstruct(HyperTermId(programs.hyperGraph.nodes.map(_.id).max)).contains(term) :| programs.toString
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
      val tree = new Tree[Identifier](root, List(new Tree[Identifier](son)))
      val programs = Programs(tree)

      val results = programs.reconstruct(HyperTermId(2)).toList

      results.size == 1 && results.contains(tree)
    }
    })
  }

  property("be able to handle a tree with 1 depth and return it") {
    check(forAll { (root: Identifier, son1: Identifier, son2: Identifier) => {
      val tree = new Tree[Identifier](root, List(new Tree[Identifier](son1), new Tree[Identifier](son2)))
      val programs = Programs(tree)

      val results = programs.reconstruct(HyperTermId(3)).toList

      results.size == 1 && results.contains(tree)
    }
    })
  }

  property("every edge is constructable") {
    check(forAll { programs: Programs =>
      programs.hyperGraph.nodes.map(programs.reconstruct).forall(_.nonEmpty)
    })
  }

  property("unknown term returns empty iterator") {
    check(forAll { programs: Programs =>
      programs.reconstruct(HyperTermId(-1)).isEmpty
    })
  }

  property("destruct the tree to the right graph") {
    check(forAll { (root: Identifier, param1: Identifier, param2: Identifier) =>
      (root != param1 && root != param2 && param1 != param2) ==> {
        val tree = new Tree[Identifier](root, List(new Tree[Identifier](param1), new Tree[Identifier](param2)))
        val hyperEdges = Set(HyperEdge(HyperTermId(3), HyperTermIdentifier(root), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
          HyperEdge(HyperTermId(1), HyperTermIdentifier(param1), List.empty, EmptyMetadata),
          HyperEdge(HyperTermId(2), HyperTermIdentifier(param2), List.empty, EmptyMetadata)
        )

        val programs = Programs(tree)

        programs.hyperGraph.edges == hyperEdges
      }

    })
  }

  property("destruct splitted term and find using pattern") {
    check(forAll { (term1: Term, term2: Term) =>
      (term1.nodes ++ term2.nodes).map(_.root).intersect(Seq("/", "id")).isEmpty ==> {
        val progs = Programs(new Tree[Identifier](new Identifier("/"), List(term1, term2)))
        val edges = progs.hyperGraph.findEdges(new HyperTermIdentifier(new Identifier("id")))
        edges.map(_.sources.head).forall(t => progs.reconstruct(t).toSeq.intersect(Seq(term1, term2)).nonEmpty)
      }
    })
  }

  property("destruct pattern has right amount of references") {
    val parser = new TranscalParser
    val pattern1 = parser("_ + _")
    check(Programs.destructPattern(pattern1).nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 2)
    val pattern2 = parser("_ + _ - _")
    check(Programs.destructPattern(pattern2).nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 3)
    val pattern3 = parser("?x ?y -> _ + x + y")
    check(Programs.destructPattern(pattern3).nodes.count(_.isInstanceOf[ReferenceTerm[HyperTermId]]) == 3)
  }
}
