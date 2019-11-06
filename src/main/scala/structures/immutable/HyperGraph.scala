package structures.immutable

import structures.generic.HyperGraphLikeGenericCompanion
import structures.{HyperEdge, HyperGraphLike}

import scala.collection.immutable


trait HyperGraph[Node, EdgeType]
  extends immutable.Set[HyperEdge[Node, EdgeType]]
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
    VocabularyHyperGraph.newBuilder[A, B]

}
