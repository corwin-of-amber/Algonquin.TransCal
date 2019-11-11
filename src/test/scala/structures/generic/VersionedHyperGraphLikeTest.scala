package structures.generic

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.HyperGraphLike.HyperEdgePattern
import structures.generic.VersionedHyperGraphLike.VersionMetadata
import structures.{EmptyMetadata, Explicit, HyperEdge, HyperGraphLike, HyperGraphLikeTest, Item}

trait VersionedHyperGraphLikeTest[Node,
  EdgeType,
  VersionedHyperGraph <: VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph] with collection.Set[HyperEdge[Node, EdgeType]],
  Pattern <: HyperGraphLike.HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperGraphLike.HyperEdgePattern[Node,EdgeType,Int]]] extends
  HyperGraphLikeTest[Node, EdgeType, VersionedHyperGraph, Pattern] {

  property("test edges are found in their versions") {
    forAll{ g: VersionedHyperGraph => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.foreach{edge =>
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Node, EdgeType, Int] = HyperEdge(Explicit[Node, Int](edge.target), Explicit[EdgeType, Int](edge.edgeType), edge.sources.map(n => Explicit[Node, Int](n)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned[Int](hyperGraph, version - 1) should not be empty
      }
    }}
  }

  property("test edges are found in versions before them") {
    forAll{ g: VersionedHyperGraph => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.foreach{edge =>
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Node, EdgeType, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version) should not be empty
      }
    }}
  }

  property("test edges are not found in versions after them") {
    forAll{ g: VersionedHyperGraph => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.foreach{edge =>
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Node, EdgeType, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version + 1) should be (empty)
      }
    }}
  }

  property("currentVersion property") {
    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType]) => {
      val newG = g + e
      g.currentVersion should be < newG.currentVersion
    }}
  }

  property("findSubgraphVersioned property") {
    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType]) => { whenever(!g.contains(e)) {
      val curVer = g.currentVersion
      val newG = g + e
      val pattern: HyperGraph[Item[Node, Int], Item[EdgeType, Int]] = HyperGraph(e.copy(target = Explicit(e.target), edgeType = Explicit(e.edgeType), sources = e.sources.map(s => Explicit(s))))
      val results = newG.findSubgraphVersioned[Int](pattern, curVer + 1)
      results should not be empty
    }}}
  }
}
