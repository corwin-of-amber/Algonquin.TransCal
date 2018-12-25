package synthesis

import org.scalatest.{FlatSpec, Matchers}
import structures.HyperEdge
import syntax.{Identifier, Tree}

class ProgramsSpec extends FlatSpec with Matchers {

  "Programs" should "destruct the tree to the right graph" in {
    val root = new Identifier(0)
    val param1 = new Identifier(1)
    val param2 = new Identifier(2)
    val tree = new Tree[Identifier](root, List(new Tree[Identifier](param1), new Tree[Identifier](param2)))
    val hyperEdge = HyperEdge(HyperTermId(1), HyperTermIdentifier(root), Seq(HyperTermIdentifier(param1), HyperTermIdentifier(param2)))

    val programs = Programs(tree)

    programs.hyperGraph.edges shouldEqual Set(hyperEdge)
  }

  it should "be able to handle a tree of one and return it" in {
    val tree = new Tree[Identifier](new Identifier(0), List(new Tree[Identifier](new Identifier(1))))
    val programs = Programs(tree)

    val results = programs.reconstruct(HyperTermId(0)).toList

    results should have size 1
    results should contain(tree)
  }

  it should "be able to handle a tree with 1 depth and return it" in {
    val tree = new Tree[Identifier](new Identifier(0), List(new Tree[Identifier](new Identifier(1)), new Tree[Identifier](new Identifier(2))))
    val programs = Programs(tree)

    val results = programs.reconstruct(HyperTermId(0)).toList

    results should have size 1
    results should contain (tree)
  }
}
