package structures.mutable

import structures.HyperEdge
import structures.generic.HyperGraphLikeGenericCompanion

import scala.collection.mutable

trait HyperGraph[Node, EdgeType]
  extends mutable.Set[HyperEdge[Node, EdgeType]]
    with structures.generic.HyperGraph[Node, EdgeType]
     with HyperGraphLike[Node, EdgeType, HyperGraph[Node, EdgeType]] {

  override def empty: HyperGraph[Node, EdgeType] = HyperGraph.empty
}

object HyperGraph extends HyperGraphLikeGenericCompanion[HyperGraph] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: collection.mutable.Builder[HyperEdge[A, B], HyperGraph[A, B]] =
    VocabularyHyperGraph.newBuilder
}
