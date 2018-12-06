package structures.immutable

import org.scalatest.{FlatSpec, Matchers}
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge


class VocabularyHyperGraphUnitSpec extends FlatSpec with Matchers {

  "TrieHyperGraph" should "add an edge with empty sources" in {
    val first = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edgeType = 'a'
    val sources = Seq.empty

    val hyperGraph = first addEdge HyperEdge(target, edgeType, sources)

    hyperGraph.nodes shouldBe Set(target)
    hyperGraph.edgeTypes shouldBe Set(edgeType)
  }

  it should "remove an edge" in {
    val first = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val edgeType = 'a'
    val sources = Seq.empty

    val hyperGraph = first addEdge HyperEdge(target, edgeType, sources) removeEdge HyperEdge(target, edgeType, sources)

    hyperGraph.nodes shouldBe empty
    hyperGraph.edgeTypes shouldBe empty
  }

  it should "merge an edge" in {
    val first = VocabularyHyperGraph.empty[Int, Char]
    val target = 1
    val newTarget = 2
    val edgeType = 'a'
    val sources = Seq.empty

    val hyperGraph = first addEdge HyperEdge(target, edgeType, sources) mergeNodes (newTarget, target)

    hyperGraph.nodes shouldBe Set(newTarget)
    hyperGraph.edgeTypes shouldBe Set(edgeType)
  }
}
