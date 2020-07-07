package synthesis.search

import structures.{HyperEdge, Metadata, SpecificMergeMetadata}
import structures.generic.HyperGraph.Match
import synthesis.search.DepthAwareSearch.DepthMetadata
import synthesis.{HyperTermId, HyperTermIdentifier}

class DepthAwareSearch(maxDepth: Int, maxEdgeDepth: Option[Int] = None, startVersioned: Boolean = false, isGoal: ActionSearchState.HyperGraph => Boolean = _ => false)
  extends NaiveSearch(startVersioned, isGoal) {

  val edgeDepth = maxEdgeDepth.getOrElse(maxDepth)

  override def postProcessors: Set[Set[HyperEdge[HyperTermId, HyperTermIdentifier]] => Set[HyperEdge[HyperTermId, HyperTermIdentifier]]] = {
    super.postProcessors + ((edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
      edges.filter(e => e.metadata.forall({
        case DepthMetadata(x) if x > edgeDepth => false
        case _ => true
      })))
  }

  def creator(matched: Match[HyperTermId, HyperTermIdentifier, Int]): Metadata = {
    val metas = matched.edges.flatMap(_.metadata.collect({
      case dm @ DepthMetadata(x) => Some(dm)
      case _ => None
    })).flatten
    if (metas.isEmpty) DepthMetadata(1)
    else DepthMetadata(metas.map(_.depth).max + 1)
  }

  override def apply(state: ActionSearchState, depth: Double): ActionSearchState = {
    state.rewriteRules.foreach(_.registerMetadataCreator(creator))
    val res = super.apply(state, depth)
    state.rewriteRules.foreach(_.unregisterMetadataCreator(creator))
    res
  }
}

object DepthAwareSearch {
  case class DepthMetadata(depth: Int) extends SpecificMergeMetadata {
    override protected def toStr: String = s"DepthMetadata($depth)"

    override def mergeSpecific(other: SpecificMergeMetadata): SpecificMergeMetadata = other match {
      case DepthMetadata(x) if x < depth => other
      case _ => this
    }
  }
}