package structures.immutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.HyperGraphLike.HyperEdgePattern
import structures._

import scala.util.Random


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers with Matchers with HyperGraphLikeTest[Int, Int, VocabularyHyperGraph[Int, Int], VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VocabularyHyperGraph[Int, Int]] = Arbitrary(integerGraphGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VocabularyHyperGraph[Int, Int] = VocabularyHyperGraph(es.toSeq: _*)

  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] = VocabularyHyperGraph(es.toSeq: _*)

  property("all constructor") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      new VocabularyHyperGraph(es).edges == es && VocabularyHyperGraph(es.toSeq: _*).edges == es
    })
  }

  property("find graph finds nothing") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(800), Seq(Ignored(), Ignored()), EmptyMetadata),
        HyperEdge(Explicit(800), Ignored(), Seq(), EmptyMetadata),
        HyperEdge(Ignored(), Ignored(), Seq(Explicit(800)), EmptyMetadata)).map(Set(_))
      edges.forall(e => {
        val pg = patterner(e)
        g.findSubgraph[Int](pg).isEmpty
      }
      )
    })
  }

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
    val found = graph.findRegexHyperEdges(pattern)
    val edges = graph.edges
    check(found == edges)
  }
}
