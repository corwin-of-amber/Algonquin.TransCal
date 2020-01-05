package structures

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Identifier

class HyperEdgeTest extends PropSpec with Matchers with ScalaCheckPropertyChecks with ParallelTestExecution {
  implicit def edgeCreator: Arbitrary[HyperEdge[HyperTermId, HyperTermIdentifier]] = Arbitrary(integerEdgesGen.map(e =>
    HyperEdge(HyperTermId(e.target), HyperTermIdentifier(Identifier(e.edgeType.toString)), e.sources.map(HyperTermId), EmptyMetadata
    )))

  property("to json and back doesnt change edge") {
    forAll { e: HyperEdge[HyperTermId, HyperTermIdentifier] =>
      e shouldEqual HyperEdge.fromJson(HyperEdge.toJson(e))
    }
  }
}
