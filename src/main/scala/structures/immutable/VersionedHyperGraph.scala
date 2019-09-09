package structures.immutable

import structures._
import structures.immutable.HyperGraph.HyperGraphPattern
import structures.immutable.VersionedHyperGraph.VersionMetadata

import scala.collection.{GenTraversableOnce, mutable}

/** This hyper graph search the most upated edges.
  *
  * @author tomer
  * @since 11/15/18
  */
class VersionedHyperGraph[Node, EdgeType] private(wrapped: HyperGraph[Node, EdgeType], val version: Long, mapByVersion: Map[Long, Set[HyperEdge[Node, EdgeType]]])
  extends WrapperHyperGraph[Node, EdgeType, VersionedHyperGraph[Node, EdgeType]](wrapped) {

  /* --- Constructors --- */

  def this(wrapped: HyperGraph[Node, EdgeType]) = {
    this(wrapped, VersionMetadata.latestVersion(wrapped.edges), VersionedHyperGraph.createMapOfVersions(wrapped))
  }

  /* --- Public Methods --- */

  def findSubgraphVersioned[Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], version: Long): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    if (version == 0) wrapped.findSubgraph[Id](hyperPattern)
    else {
      val relevantVersionGraph = HyperGraph(mapByVersion.filterKeys(_ >= version).values.flatten.toSeq:_*)
      hyperPattern.flatMap(edgePattern => {
        relevantVersionGraph.findRegexHyperEdges(edgePattern)
          .flatMap(edge => {
            val nodes = (edgePattern.target +: edgePattern.sources).zip(edge.target +: edge.sources)
            val edgeTypes = Seq((edgePattern.edgeType, edge.edgeType))
            val nodesMap = Item.itemsValueToMap(nodes)
            val edgeTypeMap = Item.itemsValueToMap(edgeTypes)
            val g = HyperGraph.mergeMap(hyperPattern, (nodesMap, edgeTypeMap))
            wrapped.findSubgraph[Id](g).map { case (foundNodes: Map[Id, Node], foundEdgeType: Map[Id, EdgeType]) => (foundNodes ++ nodesMap, foundEdgeType ++ edgeTypeMap) }
          })
      })
    }
  }

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  /**
    * Adding version to edge.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  override def +(hyperEdge: HyperEdge[Node, EdgeType]): VersionedHyperGraph[Node, EdgeType] = {
    if (contains(hyperEdge)) this
    else {
      val newHyperEdge = hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version)))
      val newMapByVersion = mapByVersion + ((version, mapByVersion(version) + newHyperEdge))
      new VersionedHyperGraph(
        wrapped.+(newHyperEdge),
        version + 1,
        newMapByVersion
      )
    }
  }

  /**
    * Adding version to edges.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edge.
    */
  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): VersionedHyperGraph[Node, EdgeType] = {
    val newEdges = hyperEdges.toSeq.filter(contains).map(hyperEdge => hyperEdge.copy(metadata = hyperEdge.metadata.merge(VersionMetadata(version))))
    if (newEdges.isEmpty) this
    else {
      val newMapByVersion = mapByVersion + ((version, mapByVersion(version) ++ newEdges))
      new VersionedHyperGraph(
        wrapped.++(newEdges),
        version + 1,
        newMapByVersion
      )
    }
  }

  /* --- Object Impl. --- */

  override lazy val toString: String = f"VersionedHyperGraph($edges)"

  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VersionedHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        val innerHyperGraph = HyperGraph(parts:_*)
        val newMapByVersion = mapByVersion + ((version, mapByVersion(version) ++ innerHyperGraph))
        new VersionedHyperGraph(innerHyperGraph, version + 1, newMapByVersion)
      }
    }
}

object VersionedHyperGraph extends HyperGraphLikeGenericCompanion[VersionedHyperGraph] {
  val STATIC_VERSION = 0L

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VersionedHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new VersionedHyperGraph(HyperGraph(parts: _*))
    }
  }

  case class VersionMetadata(version: Long) extends SpecificMergeMetadata {
    override protected def toStr: String = s"VersionMetadata($version)"

    override def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata = other match {
      case metadata: VersionMetadata => if (metadata.version < version) other else this
    }
  }

  def createMapOfVersions[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): Map[Long, Set[HyperEdge[Node, EdgeType]]] =
    edges.groupBy(edge => VersionMetadata.getEdgeVersion(edge)).withDefaultValue(Set.empty)

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
      edge.metadata.collectFirst { case v: VersionedHyperGraph.VersionMetadata => v.version }.getOrElse(0L)
    }
  }

}
