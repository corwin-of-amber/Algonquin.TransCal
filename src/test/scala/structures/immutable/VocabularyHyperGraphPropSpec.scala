package structures.immutable

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.HyperGraphLike.HyperEdgePattern
import structures._

import scala.util.Random


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers with Matchers with HyperGraphLikeTest[Int, Int, VocabularyHyperGraph[Int, Int], VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VocabularyHyperGraph[Int, Int]] = Arbitrary(integerGraphGen)
  override implicit def nodeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  override implicit def edgeTypeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VocabularyHyperGraph[Int, Int] = VocabularyHyperGraph(es.toSeq: _*)

  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] = VocabularyHyperGraph(es.toSeq: _*)

  property("all constructor") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      new VocabularyHyperGraph(es).edges  shouldEqual es
      VocabularyHyperGraph(es.toSeq: _*).edges  shouldEqual es
    }
  }
}
