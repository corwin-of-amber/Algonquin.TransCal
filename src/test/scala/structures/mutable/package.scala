package structures

import org.scalacheck.Gen
import org.scalacheck.Gen._
import scala.util.Random

package object mutable {
  def HyperEdgeGenFactory[Node, Edge](nodeSource: Gen[Node], edgeSource: Gen[Edge]): Gen[HyperEdge[Node, Edge]] = for {
    source <- nodeSource
    edge <- edgeSource
    sourcesSize <- Random.nextInt(6)
    sources <- containerOfN[Seq, Node](sourcesSize, nodeSource)
  } yield HyperEdge(source, edge, sources)

  val integerEdgesGen: Gen[HyperEdge[Int, Int]] = HyperEdgeGenFactory(oneOf(0 to 50), oneOf(0 to 20))

  def HyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[VocabularyHyperGraphWithOrder[Node, Edge]] = {
    def grapher(se: Seq[HyperEdge[Node, Edge]]): VocabularyHyperGraphWithOrder[Node, Edge] = {
      val v = new VocabularyHyperGraphWithOrder[Node, Edge]()
      for (e <- se) v.addEdge(e)
      v
    }

    containerOf[Seq, HyperEdge[Node, Edge]](edgeSource) map grapher
  }

  val integerGraphGen: Gen[VocabularyHyperGraphWithOrder[Int, Int]] = HyperGraphGenFactory(integerEdgesGen)
}
