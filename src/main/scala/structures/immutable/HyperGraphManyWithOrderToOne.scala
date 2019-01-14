package structures.immutable

import structures.{HyperEdge, HyperGraphManyWithOrderToOneLike, HyperGraphManyWithOrderToOneLikeGenericCompanion, Item}

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOne[Node, EdgeType] extends structures.HyperGraphManyWithOrderToOne[Node, EdgeType]
  with HyperGraphManyWithOrderToOneLike[Node, EdgeType, HyperGraphManyWithOrderToOne[Node, EdgeType]]

object HyperGraphManyWithOrderToOne extends HyperGraphManyWithOrderToOneLikeGenericCompanion[HyperGraphManyWithOrderToOne] {
  type HyperGraphPattern[Node, EdgeType, Id] = HyperGraphManyWithOrderToOne[Item[Node, Id], Item[EdgeType, Id]]

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], HyperGraphManyWithOrderToOne[A, B]] =
    VocabularyHyperGraph.newBuilder
}
