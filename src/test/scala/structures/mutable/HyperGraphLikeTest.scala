package structures.mutable

import structures.{HyperEdge, IdMetadata, Metadata, Uid}
import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}

import scala.collection.mutable

trait HyperGraphLikeTest[Node,
  EdgeType,
  T <: HyperGraphLike[Node, EdgeType, T] with mutable.Set[HyperEdge[Node, EdgeType]],
  Pattern <: HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperEdgePattern[Node,EdgeType,Int]]]
  extends structures.HyperGraphLikeTest[Node, EdgeType, T, Pattern] {

  def getMetadatas(t: T): Map[(Node, EdgeType, Seq[Node]), Metadata] = {
    t.edges.map(e => ((e.target, e.edgeType, e.sources), e.metadata)).toMap
  }

  property("clone keeps metadata safe") {
    forAll { g: T =>
      whenever(g.nonEmpty) {
        val metas = getMetadatas(g)
        val uid = IdMetadata(new Uid)
        val newEdge = g.head.copy(metadata = uid)
        g -= newEdge
        g += newEdge
        metas should not equal getMetadatas(g)
        metas.values should not contain uid
      }
    }
  }

  property("clone with merge keeps metadata safe") {
    forAll { (g: T, keep: Node) =>
      whenever(g.nonEmpty && g.nodes.exists(n => n != keep)) {
        val edge = g.edges.filterNot(e=>e.target == keep && e.sources.forall(_ == keep)).head
        val cloned = g.clone()
        val change = (g.nodes - keep).head
        g.mergeNodesInPlace(change, keep)

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
