package structures.generic

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.{HyperEdge, HyperGraphLike, HyperGraphLikeTest}

trait VersionedHyperGraphLikeTest[Node,
  EdgeType,
  VersionedHyperGraph <: VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph] with collection.Set[HyperEdge[Node, EdgeType]],
  Pattern <: HyperGraphLike.HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperGraphLike.HyperEdgePattern[Node,EdgeType,Int]]] extends
  HyperGraphLikeTest[Node, EdgeType, VersionedHyperGraph, Pattern] {

  property("currentVersion property") {

  }

  property("updateVersion property") {

  }

  property("findSubgraphVersioned property") {

  }

  property("swapEdgeType property") {

  }

  property("swapNode property") {

  }

}
