package structures.generic

import structures.HyperGraphLike.HyperEdgePattern
import structures._

import scala.language.higherKinds

/**
  * @author tomer
  * @since 1/14/19
  */
abstract class HyperGraphLikeGenericCompanion[+G[N, E] <: HyperGraphLike[N, E, G[N, E]] with collection.Set[HyperEdge[N, E]]] {

  type HyperGraphPattern[Node, EdgeType, Id] = HyperGraph[Item[Node, Id], Item[EdgeType, Id]]
  /** The underlying collection type with unknown element type */
  protected[this] type Graph = G[_, _]

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  def newBuilder[A, B]: scala.collection.mutable.Builder[HyperEdge[A, B], G[A, B]]

  /** An empty collection of type `$Coll[A]`
    *
    * @tparam A the type of the ${coll}'s elements
    */
  def empty[A, B]: G[A, B] = newBuilder[A, B].result()

  /** Creates a $coll with the specified elements.
    *
    * @tparam A the type of the ${coll}'s elements
    * @param elems the elements of the created $coll
    * @return a new $coll with elements `elems`
    */
  def apply[A, B](elems: HyperEdge[A, B]*): G[A, B] = {
    if (elems.isEmpty) empty[A, B]
    else {
      val b = newBuilder[A, B]
      b ++= elems
      b.result()
    }
  }

  def mergeMatch[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]], matched: HyperGraph.Match[Node, EdgeType, Id]): HyperGraph[Item[Node, Id], Item[EdgeType, Id]] = {
    val mergedNodes = matched.nodeMap.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), Hole[Node, Id](kv._1))
    })

    val mergedAll = matched.edgeMap.foldLeft(mergedNodes)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeEdgeTypes(Explicit[EdgeType, Id](kv._2), Hole[EdgeType, Id](kv._1))
    })
    mergedAll
  }

  protected def translateEdge[Node, EdgeType, Id](hyperEdgePattern: HyperEdgePattern[Node, EdgeType, Id]): HyperEdge[Node, EdgeType] = {
    hyperEdgePattern.copy(hyperEdgePattern.target.asInstanceOf[Explicit[Node, Id]].value, hyperEdgePattern.edgeType.asInstanceOf[Explicit[EdgeType, Id]].value,
      hyperEdgePattern.sources.map(s => s.asInstanceOf[Explicit[Node, Id]].value))
  }

  def fillWithNewHoles[Node, EdgeType, Id](patterns: Set[HyperEdge[Item[Node, Id], Item[EdgeType, Id]]], nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    val newTerms = patterns.flatMap(e => e.sources :+ e.target).filter(_.isInstanceOf[Hole[Node, Id]]).map((_, Explicit[Node, Id](nodeCreator()))).toMap
    val temp = patterns.map(e => e.copy(target = newTerms.getOrElse(e.target, e.target), sources = e.sources.map(s => newTerms.getOrElse(s, s))))
    temp.map(translateEdge).toSet
  }

  def fillWithNewHoles[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]],
                                           nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    val newTerms = hyperPattern.nodes.filter(_.isInstanceOf[Hole[Node, Id]]).map((_, nodeCreator()))

    newTerms.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), kv._1)
    }).map(translateEdge).toSet
  }

  def fillPattern[Node, EdgeType, Id](hyperPattern: HyperGraphPattern[Node, EdgeType, Id], matched: HyperGraph.Match[Node, EdgeType, Id], nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    // Should crash if we still have holes as its a bug
    val mergedAll = mergeMatch(hyperPattern, matched)
    fillWithNewHoles(mergedAll, nodeCreator)
  }
}