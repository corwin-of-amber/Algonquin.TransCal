package structures.mutable

import structures.HyperGraphLike.HyperEdgePattern
import structures._

import scala.language.higherKinds

/**
  * @author tomer
  * @since 1/14/19
  */
abstract class HyperGraphCompanion[+G[N, E] <: HyperGraphLike[N, E, G[N, E]] with collection.mutable.Set[HyperEdge[N, E]]] extends structures.HyperGraphCompanion[G] {

  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: scala.collection.mutable.Builder[HyperEdge[A, B], G[A, B]]

  def mergeMatch[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]], matched: structures.Match[Node, EdgeType, Id]): HyperGraph[Item[Node, Id], Item[EdgeType, Id]] = {
    matched.nodeMap.foreach(kv => hyperPattern.mergeNodesInPlace(Explicit[Node, Id](kv._2), Hole[Node, Id](kv._1)))
    matched.edgeMap.foreach(kv => hyperPattern.mergeEdgeTypesInPlace(Explicit[EdgeType, Id](kv._2), Hole[EdgeType, Id](kv._1)))
    hyperPattern
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
    newTerms.foreach(kv => hyperPattern.mergeNodesInPlace(Explicit[Node, Id](kv._2), kv._1))
    hyperPattern.map(translateEdge).toSet
  }

  def fillPattern[Node, EdgeType, Id](hyperPattern: HyperGraph[Item[Node, Id], Item[EdgeType, Id]], matched: Match[Node, EdgeType, Id], nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    // Should crash if we still have holes as its a bug
    val mergedAll = mergeMatch(hyperPattern, matched)
    fillWithNewHoles(mergedAll, nodeCreator)
  }
}