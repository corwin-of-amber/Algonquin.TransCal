package synthesis

import org.scalatest.{FlatSpec, Matchers}
import structures.HyperEdge
import syntax.Tree

class ProgramsSpec extends FlatSpec with Matchers {

  "Programs" should "destruct the tree to the right graph" in {
    val root = 0
    val param1 = 1
    val param2 = 2
    val tree = new Tree[Int](root, List(new Tree[Int](param1), new Tree[Int](param2)))
    val hyperEdge = new HyperEdge(new HyperTerm(3), new HyperTerm(root), Seq(new HyperTerm(param1), new HyperTerm(param2)))

    val programs = new Programs(tree)

    programs.hyperGraph.edges shouldEqual Set(hyperEdge)
  }

  it should "be able to handle a tree of one and return it" in {
    val tree = new Tree[Int](0)
    val programs = new Programs(tree)

    val results = programs.reconstruct(new HyperTerm(0)).toList

    results should have size 1
    results should contain(tree)
  }

  it should "be able to handle a tree with 1 depth and return it" in {
    val tree = new Tree[Int](0, List(new Tree[Int](1), new Tree[Int](2)))
    val programs = new Programs(tree)

    val results = programs.reconstruct(new HyperTerm(0)).toList

    results should have size 1
    results should contain(tree)
  }
}
