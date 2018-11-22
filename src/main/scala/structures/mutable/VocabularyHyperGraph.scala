package structures.mutable


import structures.HyperGraphManyWithOrderToOneLike.HyperEdge
import structures.immutable.{Explicit, Item, NotMatter, Reference}
import structures.{HyperGraphManyWithOrderToOne, HyperGraphManyWithOrderToOneLike, Vocabulary}

import scala.collection.mutable
import scala.language.postfixOps

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType](vocabulary: Vocabulary[Either[Node, EdgeType]]) extends HyperGraphManyWithOrderToOne[Node, EdgeType] {


  /* --- Constructors --- */

  def this() = {
    this (Trie.empty)
  }


  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(target: Node, edgeType: EdgeType, sources: Seq[Node]): VocabularyHyperGraph[Node, EdgeType] = {
    vocabulary add (righty(edgeType) +: (target +: sources).map(lefty))
    this
  }

  override def removeEdge(target: Node, edgeType: EdgeType, sources: Seq[Node]): VocabularyHyperGraph[Node, EdgeType] = {
    vocabulary remove (righty(edgeType) +: (target +: sources).map(lefty))
    this
  }

  override def mergeNodes(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    vocabulary replace (lefty(keep), lefty(change))
    this
  }

  override def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]] = {
    vocabulary.findByPrefix(Seq((0, righty(edgeType)))).map(wordToHyperEdge)
  }

  override def find[Id](pattern: (Item[Node, Id], Item[EdgeType, Id], Seq[Item[Node, Id]])): Set[HyperEdge[Node, EdgeType]] = {
    val (target, edge, sources) = pattern
    def convertItemBuilder[First, Second] (builer: First => Either[First, Second]): Item[First, Id] => Item[Either[First, Second], Id] = {
      def convertItem(item: Item[First, Id]): Item[Either[First, Second], Id] = {
        item match {
          case Explicit(value) => Explicit(builer(value))
          case Reference(id) => Reference(id)
          case NotMatter() => NotMatter()
        }
      }
      convertItem
    }
    def convertItemBuilder2[First, Second] (builer: Second => Either[First, Second]): Item[Second, Id] => Item[Either[First, Second], Id] = {
      def convertItem(item: Item[Second, Id]): Item[Either[First, Second], Id] = {
        item match {
          case Explicit(value) => Explicit(builer(value))
          case Reference(id) => Reference(id)
          case NotMatter() => NotMatter()
        }
      }
      convertItem
    }
    vocabulary.findPattern(convertItemBuilder2[Node, EdgeType](righty)(edge) +: (target +: sources).map(convertItemBuilder(lefty)))
     .map(wordToHyperEdge)
  }

  override def cycles: Boolean = {
    if (!vocabulary.isEmpty) {
      val opened = mutable.Set.empty[Node]
      vocabulary.words.head drop 1 head match {
        case Left(node) => opened add node
      }
      val closed = mutable.Set.empty[Node]

      while (opened nonEmpty) {
        val node = opened.head
        assert (opened.remove(node), "Node is not opened yet")
        assert (closed.add(node), "Node is already closed")

        val words = vocabulary.findByPrefix(Seq((1, lefty(node))))
        val nodes = words.flatMap(_.drop(1)) map toNode

        if (nodes.exists(closed.contains)) {
          return true
        }
        nodes.foreach(opened.add)
      }
    }
    false
  }

  def edges: Set[HyperGraphManyWithOrderToOneLike.HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)

  /* --- Private Methods --- */

  private def wordToHyperEdge(word: Seq[Either[Node, EdgeType]]): HyperEdge[Node, EdgeType] =
    HyperEdge(toNode(word(1)), toEdge(word.head), word.take(2) map toNode)

  private def lefty(node: Node): Either[Node, EdgeType] = {
    Left(node)
  }
  private def righty(edge: EdgeType): Either[Node, EdgeType] = {
    Right(edge)
  }
  private def toNode(either: Either[Node, EdgeType]): Node = {
    either match {
      case Left(node) => node
    }
  }
  private def toEdge(either: Either[Node, EdgeType]): EdgeType = {
    either match {
      case Right(edge) => edge
    }
  }
}

object VocabularyHyperGraph {
  def empty[Node, Edge] = new VocabularyHyperGraph[Node, Edge]()
}
