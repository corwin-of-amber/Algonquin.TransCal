package structures.immutable

import structures._
import structures.immutable.VersionedHyperGraph.VersionMetadata

import scala.collection.mutable

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: CompactHyperGraph[Node, EdgeType], version: Long)
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  /* --- Constructors --- */

  def this(wrapped: CompactHyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges))
  }

  /* --- Public Methods --- */

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphManyWithOrderToOne.HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    hyperPattern.edges.flatMap(edgePattern => {
      find(edgePattern)
        .filter(edge => VersionMetadata.getEdgeVersion(edge) >= version)
        .flatMap(edge => {
          val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
          val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
          val nodesMap = Item.itemsValueToMap(nodes)
          val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
          val g = HyperGraphManyWithOrderToOneLike.mergeMap(hyperPattern, (nodesMap, edgeTypeMap))
          wrapped.findSubgraph[Id](g).map(t => (t._1 ++ nodesMap, t._2 ++ edgeTypeMap))
        })
    })
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] = {
    new VersionedHyperGraph(wrapped.addEdge(hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))), version + 1)
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] = {
    new VersionedHyperGraph(wrapped.addEdges(hyperEdges.map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version))))), version + 1)
  }

  /* --- Object Impl. --- */

  override def toString: String = f"VersionedHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.LazyBuilder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] {
      override def result(): VersionedHyperGraph[Node, EdgeType] = {
        new VersionedHyperGraph(new CompactHyperGraph(parts.flatten.toSet), version + 1)
      }
    }
}

object VersionedHyperGraph extends HyperGraphManyWithOrderToOneLikeGenericCompanion[VersionedHyperGraph] {
  val STATIC_VERSION = 0L

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VersionedHyperGraph[A, B]] = new mutable.LazyBuilder[HyperEdge[A, B], VersionedHyperGraph[A, B]] {
    override def result(): VersionedHyperGraph[A, B] = {
      new VersionedHyperGraph(CompactHyperGraph(parts.flatten: _*))
    }
  }

  case class VersionMetadata(version: Long) extends SpecificMergeMetadata {
    override protected def toStr: String = "VersionMetadata"

    override def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata = other match {
      case metadata: VersionMetadata => if (metadata.version < version) other else this
    }
  }

  object VersionMetadata {
    /**
      *
      * @param edges To check with
      * @tparam Node     node type
      * @tparam EdgeType edge type
      * @return
      */
    def latestVersion[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): Long = (edges.map(getEdgeVersion) + 0L).min

    /**
      * Getting the edge version
      *
      * @param edge Edge to check with
      * @tparam Node     node type
      * @tparam EdgeType edge type
      * @return
      */
    def getEdgeVersion[Node, EdgeType](edge: HyperEdge[Node, EdgeType]): Long = {
      edge.metadata.filter(_.isInstanceOf[VersionMetadata]).map(_.asInstanceOf[VersionMetadata].version).headOption.getOrElse(0L)
    }
  }

}
