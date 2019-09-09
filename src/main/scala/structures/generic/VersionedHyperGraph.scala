package structures.generic

import structures.{HyperEdge, SpecificMergeMetadata}

object VersionedHyperGraph {
  val STATIC_VERSION = 0L

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
