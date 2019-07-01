package structures.immutable

import structures.immutable.HyperGraphManyWithOrderToOneLike.{HyperEdgePattern, HyperGraphPattern}
import structures.{Explicit, Hole, HyperEdge, Item}

import scala.collection.{immutable, GenTraversableOnce, SetLike}

/** A hyper graph from many to one.
  *
  * @author tomer
  * @since 11/15/18
  */
trait HyperGraphManyWithOrderToOneLike[Node, EdgeType, +This <: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This] with immutable.Set[HyperEdge[Node, EdgeType]]]
  extends SetLike[HyperEdge[Node, EdgeType], This] {

  /** Finds all the edges with the EdgeType
    *
    * @param edgeType to search.
    * @return correspond edges.
    */
  def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]] = edges.filter(_.edgeType == edgeType)

  /** Find a pattern of an edge in the graph.
    *
    * @param pattern The pattern of an edge.
    * @tparam Id A reference type to show a wanted connection in the pattern.
    * @return The matched edges
    */
  def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(HyperEdge[Node, EdgeType], Map[Id, Node], Map[Id, EdgeType])]
  def findRegexHyperEdges[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[HyperEdge[Node, EdgeType]] = findRegex(pattern).map(_._1)
  def findRegexMaps[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(Map[Id, Node], Map[Id, EdgeType])] = findRegex(pattern).map(t => (t._2, t._3))
  override def contains(elem: HyperEdge[Node, EdgeType]): Boolean = findRegex(HyperEdge(Explicit(elem.target), Explicit(elem.edgeType), elem.sources.map(Explicit(_)), elem.metadata)).nonEmpty

  /** Finds subgraphs by a pattern graph.
    *
    * @param hyperPattern The pattern graph to match with
    * @tparam Id      A reference type to show a wanted connection in the pattern.
    * @tparam Pattern The type of the pattern subgraph
    * @return The matched references.
    */
  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])]

  /**
    * @return all the nodes in the hyper graph.
    */
  def nodes: Set[Node] = edges.flatMap(edge => edge.target +: edge.sources)

  /**
    * @return all the edge types in the hyper graph.
    */
  def edgeTypes: Set[EdgeType] = edges.flatMap(edge => Set(edge.edgeType))

  /**
    * @return all the edges in the hyper graph.
    */
  def edges: Set[HyperEdge[Node, EdgeType]]

  /**
    * @return all the edges in the hyper graph ordered by target.
    */
  def edgesOrdered(implicit ordering: Ordering[HyperEdge[Node, EdgeType]]): Seq[HyperEdge[Node, EdgeType]] = edges.toSeq.sorted(ordering)

  /** Adds an edge to the hyper graph.
    *
    * @param hyperEdge The edge to add.
    * @return The new hyper graph with the edge.
    */
  def +(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Adds edges to the hyper graph.
    *
    * @param hyperEdges The edges to add.
    * @return The new hyper graph with the edges.
    */
  def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): This

  /** Removes an edge from the hyper graph.
    *
    * @param hyperEdge The edge to remove.
    * @return The new hyper graph without the edge.
    */
  def -(hyperEdge: HyperEdge[Node, EdgeType]): This

  /** Merges two node to one.
    *
    * @param keep   The node to change to.
    * @param change The node to change from.
    * @return The new graph after the change.
    */
  def mergeNodes(keep: Node, change: Node): This

  /** Merges two edge types to one.
    *
    * @param keep   The edge to change to.
    * @param change The edge to change from.
    * @return The new graph after the change.
    */
  def mergeEdgeTypes(keep: EdgeType, change: EdgeType): This

  /* --- IterableLike Impl. --- */

  override def iterator: Iterator[HyperEdge[Node, EdgeType]] = edges.iterator

  /* --- Object Impl. --- */

  override def hashCode(): Int = edges.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case x: HyperGraphManyWithOrderToOneLike[Node, EdgeType, This] => x.edges == edges
    case _ => false
  }

}


object HyperGraphManyWithOrderToOneLike {
  def mergeMap[Node, EdgeType, Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern,
                                                                                              maps: (Map[Id, Node], Map[Id, EdgeType])): Pattern = {
    val (nodeMap, edgeMap) = maps
    val mergedNodes = nodeMap.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), Hole[Node, Id](kv._1))
    })

    val mergedAll = edgeMap.foldLeft(mergedNodes)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeEdgeTypes(Explicit[EdgeType, Id](kv._2), Hole[EdgeType, Id](kv._1))
    })
    mergedAll
  }

  def fillWithNewHoles[Node, EdgeType, Id,
  Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern, nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    val newTerms = hyperPattern.nodes.filter(_.isInstanceOf[Hole[Node, Id]]).map((_, nodeCreator()))

    def translateEdge(hyperEdgePattern: HyperEdgePattern[Node, EdgeType, Id]): HyperEdge[Node, EdgeType] = {
      hyperEdgePattern.copy(hyperEdgePattern.target.asInstanceOf[Explicit[Node, Id]].value, hyperEdgePattern.edgeType.asInstanceOf[Explicit[EdgeType, Id]].value,
        hyperEdgePattern.sources.map(s => s.asInstanceOf[Explicit[Node, Id]].value))
    }

    newTerms.foldLeft(hyperPattern)((graph, kv) => {
      // From each map create new edges from the destination graph
      graph.mergeNodes(Explicit[Node, Id](kv._2), kv._1)
    }).map(translateEdge(_)).toSet
  }

  def fillPattern[Node, EdgeType, Id,
    Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern,
                                                               maps: (Map[Id, Node], Map[Id, EdgeType]),
                                                               nodeCreator: () => Node): Set[HyperEdge[Node, EdgeType]] = {
    // Should crash if we still have holes as its a bug
    val mergedAll = mergeMap[Node, EdgeType, Id, Pattern](hyperPattern, maps)
    fillWithNewHoles[Node, EdgeType, Id, Pattern](mergedAll, nodeCreator)
  }

  // Shortcuts
  type HyperEdgePattern[Node, EdgeType, Id] = HyperEdge[Item[Node, Id], Item[EdgeType, Id]]
  type HyperGraphPattern[Node, EdgeType, Id, +This <: HyperGraphPattern[Node, EdgeType, Id, This] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]] = HyperGraphManyWithOrderToOneLike[Item[Node, Id], Item[EdgeType, Id], This]
}