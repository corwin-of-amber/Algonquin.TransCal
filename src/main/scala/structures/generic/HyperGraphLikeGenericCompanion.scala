package structures.generic

import structures.{HyperEdge, HyperGraphLike}

import scala.language.higherKinds

/**
  * @author tomer
  * @since 1/14/19
  */
abstract class HyperGraphLikeGenericCompanion[+G[N, E] <: HyperGraphLike[N, E, G[N, E]] with collection.Set[HyperEdge[N ,E]]] {


  /** The underlying collection type with unknown element type */
  protected[this] type Graph = G[_, _]

  /** The default builder for `$Coll` objects.
    *  @tparam A      the type of the ${coll}'s elements
    */
  def newBuilder[A, B]: scala.collection.mutable.Builder[HyperEdge[A, B], G[A, B]]

  /** An empty collection of type `$Coll[A]`
    *  @tparam A      the type of the ${coll}'s elements
    */
  def empty[A, B]: G[A, B] = newBuilder[A, B].result()

  /** Creates a $coll with the specified elements.
    *  @tparam A      the type of the ${coll}'s elements
    *  @param elems  the elements of the created $coll
    *  @return a new $coll with elements `elems`
    */
  def apply[A, B](elems: HyperEdge[A, B]*): G[A, B] = {
    if (elems.isEmpty) empty[A, B]
    else {
      val b = newBuilder[A, B]
      b ++= elems
      b.result()
    }
  }
}