package structures.immutable

import structures.HyperGraphManyWithOrderToOneLike.HyperGraphPattern
import structures._
import structures.immutable.VersionedHyperGraph.VersionMetadata

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private (wrapped: CompactHyperGraph[Node, EdgeType])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  /* --- Private Members --- */

  private lazy val latestVersion = (wrapped.edges
    .flatMap(_.metadata.find(_.isInstanceOf[VersionMetadata]).map(_.asInstanceOf[VersionMetadata].version)) + 0)
    .max + 1

  /* --- Public Methods --- */

  def findSubgraphVersioned[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern]](hyperPattern: Pattern, version: Int): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    def aaa[T](tuples: Seq[(Item[T, Id], T)]): Map[Id, T] = {
      tuples.filter(t => t._1.isInstanceOf[Hole[Node, Id]])
        .map(t => (t._1.asInstanceOf[Hole[Node, Id]].id, t._2)).toMap
    }

    hyperPattern.edges.flatMap(edgePattern => {
      wrapped.find(edgePattern).filter(edge => edge.metadata.filter(_.isInstanceOf[VersionMetadata]).map(_.asInstanceOf[VersionMetadata].version).head > version)
        .map(edge => {
          val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
          val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
          val b = HyperGraphManyWithOrderToOneLike.mergeMap(hyperPattern, (aaa(nodes), aaa(edgeTypes)))
          b
        }).flatMap(wrapped.findSubgraph[Id, Pattern])
    })
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] = {
    super.addEdge(hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(latestVersion))))
  }

  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] = {
    super.addEdges(hyperEdges.map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(latestVersion)))))
  }

  /* --- Object Impl. --- */

  override def toString: String = f"VersionedHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.LazyBuilder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] {
      override def result(): VersionedHyperGraph[Node, EdgeType] = {
        new VersionedHyperGraph(new CompactHyperGraph(parts.flatten.toSet))
      }
    }
}

object VersionedHyperGraph extends HyperGraphManyWithOrderToOneLikeGenericCompanion[VersionedHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VersionedHyperGraph[A, B]]= new mutable.LazyBuilder[HyperEdge[A, B], VersionedHyperGraph[A, B]] {
    override def result(): VersionedHyperGraph[A, B] = {
      new VersionedHyperGraph(CompactHyperGraph(parts.flatten:_*))
    }
  }

  case class VersionMetadata(version: Int) extends SpecificMergeMetadata {
    override protected def toStr: String = "VersionMetadata"

    override def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata = other match {
      case metadata: VersionMetadata => if (metadata.version < version) other else this
    }
  }
}
