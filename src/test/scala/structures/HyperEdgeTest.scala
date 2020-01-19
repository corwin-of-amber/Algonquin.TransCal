package structures

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import synthesis.{HyperTermId, HyperTermIdentifier}

class HyperEdgeTest extends PropSpec with Matchers with ScalaCheckPropertyChecks with ParallelTestExecution {
  implicit val arbitraryEdges = Arbitrary(hyperEdgesGen)

  property("If graphs are equal then hash is equal") {
    forAll { e: HyperEdge[HyperTermId, HyperTermIdentifier] =>
      e shouldEqual HyperEdge.fromJson(HyperEdge.toJson(e))
    }
  }
}
