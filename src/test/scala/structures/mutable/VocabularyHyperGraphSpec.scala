package structures.mutable

import org.scalatest.{FlatSpec, Matchers}
import structures.HyperGraphManyWithOrderToOne


class VocabularyHyperGraphSpec extends FlatSpec with Matchers {

  "TrieHyperGraph" should "add an edge with empty sources" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Char] = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edge = 'a'
    val sources = Seq.empty

    hyperGraph addEdge (target, edge, sources)

    hyperGraph.nodes shouldBe Set(target)
    hyperGraph.edges shouldBe Set(edge)
  }

  it should "remove an edge" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Char] = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edge = 'a'
    val sources = Seq.empty

    hyperGraph addEdge (target, edge, sources)
    hyperGraph removeEdge (target, edge, sources)

    hyperGraph.nodes shouldBe empty
    hyperGraph.edges shouldBe empty
  }

  it should "merge an edge" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Char] = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val newTarget = 2
    val edge = 'a'
    val sources = Seq.empty

    hyperGraph addEdge (target, edge, sources)
    hyperGraph mergeNodes (newTarget, target)

    hyperGraph.nodes shouldBe Set(newTarget)
    hyperGraph.edges shouldBe Set(edge)
  }

  it should "has no cycle when empty" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Int] = VocabularyHyperGraph.empty[Int, Int]

    hyperGraph.cycles shouldBe false
  }

  it should "has cycle" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Char] = VocabularyHyperGraph.empty[Int, Char]

    val node1 = 1
    val node2= 2
    val edge1 = 'a'
    val edge2 = 'b'

    hyperGraph addEdge (node1, edge1, Seq(node2))
    hyperGraph addEdge (node2, edge2, Seq(node1))

    hyperGraph.cycles shouldBe true
  }

  it should "has a complicated cycle" in {
    val hyperGraph: HyperGraphManyWithOrderToOne[Int, Char] = VocabularyHyperGraph.empty[Int, Char]

    val node1 = 1
    val node2 = 2
    val node3 = 3
    val node4 = 4
    val edge1 = 'a'

    hyperGraph addEdge (node1, edge1, Seq(node2, node4))
    hyperGraph addEdge (node2, edge1, Seq(node3, node4))
    hyperGraph addEdge (node3, edge1, Seq(node1, node4))

    hyperGraph.cycles shouldBe true
  }
}
