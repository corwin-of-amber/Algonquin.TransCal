package structures.mutable

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Prop.forAll
import org.scalatest.{FlatSpec, Matchers}
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge


class VocabularyHyperGraphUnitSpec extends FlatSpec with Matchers {

  "TrieHyperGraph" should "add an edge with empty sources" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edgeType = 'a'
    val sources = Seq.empty

    hyperGraph addEdge HyperEdge(target, edgeType, sources)

    hyperGraph.nodes shouldBe Set(target)
    hyperGraph.edgeTypes shouldBe Set(edgeType)
  }

  it should "remove an edge" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edgeType = 'a'
    val sources = Seq.empty

    hyperGraph addEdge HyperEdge(target, edgeType, sources)
    hyperGraph removeEdge HyperEdge(target, edgeType, sources)

    hyperGraph.nodes shouldBe empty
    hyperGraph.edgeTypes shouldBe empty
  }

  it should "merge an edge" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val newTarget = 2
    val edgeType = 'a'
    val sources = Seq.empty

    hyperGraph addEdge HyperEdge(target, edgeType, sources)
    hyperGraph mergeNodes (newTarget, target)

    hyperGraph.nodes shouldBe Set(newTarget)
    hyperGraph.edgeTypes shouldBe Set(edgeType)
  }

  it should "has no cycle when empty" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Int]

    hyperGraph.cycles shouldBe false
  }

  it should "has cycle" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Char]

    val node1 = 1
    val node2= 2
    val edgeType1 = 'a'
    val edgeType2 = 'b'

    hyperGraph addEdge HyperEdge(node1, edgeType1, Seq(node2))
    hyperGraph addEdge HyperEdge(node2, edgeType2, Seq(node1))

    hyperGraph.cycles shouldBe true
  }

  it should "has a complicated cycle" in {
    val hyperGraph = VocabularyHyperGraph.empty[Int, Char]

    val node1 = 1
    val node2 = 2
    val node3 = 3
    val node4 = 4
    val edgeType = 'a'

    hyperGraph addEdge HyperEdge(node1, edgeType, Seq(node2, node4))
    hyperGraph addEdge HyperEdge(node2, edgeType, Seq(node3, node4))
    hyperGraph addEdge HyperEdge(node3, edgeType, Seq(node1, node4))

    hyperGraph.cycles shouldBe true
  }
}
