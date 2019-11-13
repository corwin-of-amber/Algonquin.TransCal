package structures.mutable

import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}
import structures.{HyperEdge, IdMetadata, Uid}

import scala.collection.mutable

trait HyperGraphLikeTest[Node,
  EdgeType,
  T <: HyperGraphLike[Node, EdgeType, T] with mutable.Set[HyperEdge[Node, EdgeType]],
  Pattern <: HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperEdgePattern[Node,EdgeType,Int]]]
  extends structures.HyperGraphLikeTest[Node, EdgeType, T, Pattern] {

  property("clone keeps metadata safe") {
    forAll { g: T =>
      whenever(g.nonEmpty) {
        val uid = IdMetadata(new Uid)
        val oldEdge = g.head
        val newEdge = oldEdge.copy(metadata = uid)
        val cloned = g.clone()

        g += newEdge

        val optNew = g.find(e => e == newEdge)
        val optOld = cloned.find(e => e == newEdge)
        optNew.get.metadata should not equal(optOld.get.metadata)
      }
    }
  }

  property("clone with merge keeps metadata safe") {
    forAll { (g: T, keep: Node) =>
      whenever(g.nonEmpty && g.nodes.exists(n => n != keep)) {
        val edge = g.edges.filterNot(e=>e.target == keep && e.sources.forall(_ == keep)).head
        val cloned = g.clone()
        val change = (g.nodes - keep).head
        g.mergeNodesInPlace(keep, change)

        noException should be thrownBy cloned.find(e => e == edge)
      }
    }
  }

  property("clone returns new") {
    forAll { g: T =>
      whenever(g.nonEmpty) {
        val cloned = g.clone()
        cloned shouldEqual g
        cloned should not (be theSameInstanceAs g)
      }
    }
  }

  ignore("mergeEdgeTypesInPlace property") {

  }

  ignore("mergeNodesInPlace property") {

  }

}
