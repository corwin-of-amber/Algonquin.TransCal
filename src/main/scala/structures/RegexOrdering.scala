package structures

import structures.HyperGraphLike.HyperEdgePattern

/** Regex
  *
  * @param countNodes
  * @param countEdgeTypes
  * @tparam Node
  * @tparam EdgeType
  * @tparam Id
  */
class RegexOrdering[Node, EdgeType, Id](countNodes: Map[Item[Node, Id], Int], countEdgeTypes: Map[Item[EdgeType, Id], Int]) extends Ordering[HyperEdgePattern[Node, EdgeType, Id]] {
  private def numberOfExplicits(nodes: Set[Item[Node, Id]], edgeType: Item[EdgeType, Id]): Int = {
    (if (edgeType.isInstanceOf[Explicit[EdgeType, Id]]) 1 else 0) +
      nodes.count(x => x.isInstanceOf[Explicit[Node, Id]])
  }
  private def numberOfHoles(nodes: Set[Item[Node, Id]], edgeType: Item[EdgeType, Id]): Int = {
    (if (edgeType.isInstanceOf[Hole[EdgeType, Id]]) 1 else 0) +
      nodes.count(x => x.isInstanceOf[Hole[Node, Id]])
  }
  private def fills(nodes: Set[Item[Node, Id]], edgeType: Item[EdgeType, Id]): Int = {
    countEdgeTypes.getOrElse(edgeType, 0) +
      nodes.map(countNodes.getOrElse(_, 0)).sum
  }
  override def compare(x: HyperEdgePattern[Node, EdgeType, Id], y: HyperEdgePattern[Node, EdgeType, Id]): Int = {
    val xSet = ((x.target +: x.sources).toSet, x.edgeType)
    val ySet = ((y.target +: y.sources).toSet, y.edgeType)
    fills(xSet._1, xSet._2).compareTo(fills(ySet._1, ySet._2)) match {
      case 0 =>
        numberOfExplicits(xSet._1, xSet._2).compareTo(numberOfExplicits(ySet._1, ySet._2)) match {
          case 0 =>
            numberOfHoles(xSet._1, xSet._2).compareTo(numberOfHoles(ySet._1, ySet._2)) match {
              case 0 => 0
              case decided if decided != 0 => decided * -1
            }
          case decided if decided != 0 => decided
        }
      case decided if decided != 0 => decided
    }

  }
}
