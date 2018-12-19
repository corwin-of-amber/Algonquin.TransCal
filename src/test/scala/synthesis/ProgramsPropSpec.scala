package synthesis

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}

class ProgramsPropSpec extends PropSpec with Checkers {

  implicit val identifierCreator = Arbitrary(identifierGen)
  implicit val termsCreator = Arbitrary(identifierTreesGen)
  implicit val programsCreator = Arbitrary(programsGen)

  property("main program is in reconstruct") {
    check(forAll { term: Term => {
      val programs = Programs(term)
      programs.reconstruct(HyperTermId(1)).contains(term) :| programs.toString
    }
    })
  }

  property("every node is constructable") {
    check(forAll { programs: Programs =>
      programs.hyperGraph.nodes.map(programs.reconstruct).forall(_.nonEmpty)
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
    check(forAll { (root: Identifier, param1: Identifier, param2:Identifier) =>
      (root != param1 && root != param2  && param1 != param2) ==> {
        val tree = new Tree[Identifier](root, List(new Tree[Identifier](param1), new Tree[Identifier](param2)))
        val hyperEdge = HyperEdge(HyperTermId(1), HyperTermIdentifier(root), Seq(HyperTermIdentifier(param1), HyperTermIdentifier(param2)))

        val programs = Programs(tree)

        programs.hyperGraph.edges == Set(hyperEdge)
      }

    })
  }
}
