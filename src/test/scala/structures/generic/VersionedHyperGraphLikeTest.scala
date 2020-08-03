package structures.generic

import structures.HyperGraphLike.HyperEdgePattern
import structures.{EmptyMetadata, Explicit, HyperEdge, HyperGraph, HyperGraphLike, HyperGraphLikeTest, Item}

trait VersionedHyperGraphLikeTest[Node,
  EdgeType,
  VersionedHyperGraph <: VersionedHyperGraphLike[Node, EdgeType, VersionedHyperGraph] with collection.Set[HyperEdge[Node, EdgeType]],
  Pattern <: HyperGraphLike.HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperGraphLike.HyperEdgePattern[Node, EdgeType, Int]]] extends
  HyperGraphLikeTest[Node, EdgeType, VersionedHyperGraph, Pattern] {

  property("test edges are found in their versions") {
    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType]) =>
      whenever(!g.contains(e)) {
        val newG = g + e
        val regexEdge: HyperEdgePattern[Node, EdgeType, Int] =
          HyperEdge(Explicit[Node, Int](e.target), Explicit[EdgeType, Int](e.edgeType), e.sources.map(n => Explicit[Node, Int](n)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        newG.findSubgraphVersioned[Int](hyperGraph) should not be empty
      }
    }
  }

//  property("test edges are found in versions before them") {
//    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType], e1: HyperEdge[Node, EdgeType]) =>
//      whenever(g.nonEmpty && e1 != e && !g.contains(e) && !g.contains(e1)) {
//        val curVer = g.currentVersion
//        val newG = g + e
//        val newnewG = newG + e1
//        val regexEdge: HyperEdgePattern[Node, EdgeType, Int] =
//          HyperEdge(Explicit[Node, Int](e1.target), Explicit[EdgeType, Int](e1.edgeType), e1.sources.map(n => Explicit[Node, Int](n)), EmptyMetadata)
//        val hyperGraph = HyperGraph(regexEdge)
//        newnewG.findSubgraphVersioned[Int](hyperGraph, curVer) should not be empty
//      }
//    }
//  }

  property("test edges are not found in versions after them") {
    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType]) =>
      whenever(g.nonEmpty && !g.contains(e)) {
        val edges = g.edges
        val newG = g + e
        edges.foreach { edge =>
          val regexEdge: HyperEdgePattern[Node, EdgeType, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
          val hyperGraph = HyperGraph(regexEdge)
          newG.findSubgraphVersioned(hyperGraph) should be(empty)
        }
      }
    }
  }

  property("findSubgraphVersioned property") {
    forAll { (g: VersionedHyperGraph, e: HyperEdge[Node, EdgeType]) => {
      whenever(!g.contains(e)) {
        val newG = g + e
        val pattern: HyperGraph[Item[Node, Int], Item[EdgeType, Int]] = HyperGraph(e.copy(target = Explicit(e.target), edgeType = Explicit(e.edgeType), sources = e.sources.map(s => Explicit(s))))
        val results = newG.findSubgraphVersioned[Int](pattern)
        results should not be empty
      }
    }
    }
  }
}
