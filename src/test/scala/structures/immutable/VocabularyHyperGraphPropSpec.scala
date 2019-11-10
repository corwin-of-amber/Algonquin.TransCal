package structures.immutable

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.HyperGraphLike.HyperEdgePattern
import structures._


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers with Matchers with HyperGraphLikeTest[Int, Int, VocabularyHyperGraph[Int, Int], VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VocabularyHyperGraph[Int, Int]] = Arbitrary(integerGraphGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VocabularyHyperGraph[Int, Int] = VocabularyHyperGraph(es.toSeq: _*)

  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] = VocabularyHyperGraph(es.toSeq: _*)

  property("all constructor") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      new VocabularyHyperGraph(es).edges  shouldEqual es
      VocabularyHyperGraph(es.toSeq: _*).edges  shouldEqual es
    }
  }

  property("find graph finds nothing") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(800), Seq(Ignored(), Ignored()), EmptyMetadata),
        HyperEdge(Explicit(800), Ignored(), Seq(), EmptyMetadata),
        HyperEdge(Ignored(), Ignored(), Seq(Explicit(800)), EmptyMetadata)).map(Set(_))
      edges.foreach(e => {
        val pg = patterner(e)
        g.findSubgraph[Int](pg) should be (empty)
      })
    }
  }

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored())), EmptyMetadata)
    val found = graph.findRegexHyperEdges(pattern)
    val edges = graph.edges
    check(found == edges)
  }
}
