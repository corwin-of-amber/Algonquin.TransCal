import org.scalacheck.Gen
import org.scalacheck.Gen._
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Identifier

import scala.util.Random

package object structures {
  def HyperEdgeGenFactory[Node, Edge](nodeSource: Gen[Node], edgeSource: Gen[Edge]): Gen[HyperEdge[Node, Edge]] = for {
    source <- nodeSource
    edge <- edgeSource
    sourcesSize <- Random.nextInt(6)
    sources <- containerOfN[Seq, Node](sourcesSize, nodeSource)
  } yield HyperEdge(source, edge, sources, EmptyMetadata)

  val integerEdgesGen: Gen[HyperEdge[Int, Int]] = HyperEdgeGenFactory(oneOf(0 to 50), oneOf(0 to 20))
  val hyperEdgesGen: Gen[HyperEdge[HyperTermId, HyperTermIdentifier]] = integerEdgesGen.map(e => e.copy(target = HyperTermId(e.target), edgeType=HyperTermIdentifier(Identifier(e.edgeType.toString)), sources=e.sources.map(HyperTermId)))
}
