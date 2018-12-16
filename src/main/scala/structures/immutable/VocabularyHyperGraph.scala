package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphManyWithOrderToOneLike
import structures.HyperGraphManyWithOrderToOneLike._

import scala.language.postfixOps

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private (vocabulary: Vocabulary[Either[Node, EdgeType]])
  extends HyperGraphManyWithOrderToOne[Node, EdgeType]
    with HyperGraphManyWithOrderToOneLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]] with LazyLogging {


  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Add edge")
    new VocabularyHyperGraph(vocabulary add hyperEdgeToWord(hyperEdge))
  }

  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VocabularyHyperGraph[Node, EdgeType] = {
    hyperEdges.foldLeft(this)((g, e) => g.addEdge(e))
  }

  override def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Remove edge")
    new VocabularyHyperGraph(vocabulary remove hyperEdgeToWord(hyperEdge))
  }

  override def mergeNodes(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge nodes")
    new VocabularyHyperGraph(vocabulary replace (Left(keep), Left(change)))
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    new VocabularyHyperGraph(vocabulary replace (Right(keep), Right(change)))
  }

  override def findEdges(edgeType: EdgeType): Set[HyperEdge[Node, EdgeType]] = {
    logger.trace("Find edges")
    vocabulary.findPatternPrefix[Int](Seq(Explicit(Right(edgeType)), Ignored())).map(wordToHyperEdge)
  }

  override def find[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[HyperEdge[Node, EdgeType]] = {
    logger.trace("Find pattern")
    def convertNode(item: Item[Node, Id]): Item[Either[Node, EdgeType], Id] = {
      item match {
        case Explicit(value) => Explicit(Left(value))
        case Hole(id) => Hole(id)
        case Ignored() => Ignored()
      }
    }
    def convertEdgeType(item: Item[EdgeType, Id]): Item[Either[Node, EdgeType], Id] = {
      item match {
        case Explicit(value) => Explicit(Right(value))
        case Hole(id) => Hole(id)
        case Ignored() => Ignored()
      }
    }
    vocabulary.findPattern(convertEdgeType(pattern.edgeType) +: (pattern.target +: pattern.sources).map(convertNode))
     .map(wordToHyperEdge)
  }

  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern]](hyperPattern: Pattern): Set[Map[Id, Either[Node, EdgeType]]] = {
    logger.trace("Find subgraph")
    type SubPattern = HyperEdgePattern[Node, EdgeType, Id]
    type ReferencesMap = Map[Id, Either[Node, EdgeType]]
    /** Creating a new references map from known hyper edge and pattern.
      * @param knownEdge The known edge
      * @param pattern The pattern
      * @return A map of the references in pattern to the values in knownPattern.
      */
    def hyperEdgeAndTemplateToReferencesMap(knownEdge: HyperEdge[Node, EdgeType], pattern: SubPattern): ReferencesMap = {
      val nodesRefs = (pattern.target +: pattern.sources).zip(knownEdge.target +: knownEdge.sources).filter(a=>a._1.isInstanceOf[Hole[Node, Id]]).map(a=>a._1 match {
        case Hole(id) => (id, Left[Node, EdgeType](a._2))
      })
      val edgeTypeRef = pattern.edgeType match {
        case Hole(id) => Some((id,  Right[Node, EdgeType](knownEdge.edgeType)))
        case _ => None
      }
      val temp = (nodesRefs ++ edgeTypeRef).toMap

      temp
    }

    /** Fills the pattern with known references.
      * @param pattern The pattern to fill
      * @param referencesMap The known map
      * @return A filled pattern
      */
    def fillReferences(pattern: SubPattern, referencesMap: ReferencesMap): SubPattern = {
      def convert[A](b: Either[Node, EdgeType] => A, item: Item[A, Id]): Item[A, Id] = {
        item match {
          case Hole(id) => referencesMap.get(id).map(b).map(Explicit[A, Id]).getOrElse(item)
          case _ => item
        }
      }
      val newTarget = convert[Node]({ case Left(node) => node }, pattern.target)
      val newEdgeType = convert[EdgeType]({ case Right(edgeType) => edgeType }, pattern.edgeType)
      val newSources = pattern.sources.map(convert[Node]({ case Left(node) => node }, _))
      HyperEdge(newTarget, newEdgeType, newSources)
    }

    /** Creating reference map for a lot of matches.
      * @param itemEdges pattern edges.
      * @param referencesMap current reference map
      * @return a nee reference map
      */
    def getReferencesMap(itemEdges: Seq[SubPattern], referencesMap: ReferencesMap): Set[ReferencesMap] = {
      itemEdges match {
        case Nil => Set(referencesMap)
        case itemEdge +: left =>
          val filledEdge= fillReferences(itemEdge, referencesMap)
          (for (hyperEdge <- this.find(filledEdge)) yield {
            val newReferences = hyperEdgeAndTemplateToReferencesMap(hyperEdge, filledEdge) ++ referencesMap
            getReferencesMap(left, newReferences)
          }).flatten
      }
    }
    getReferencesMap(hyperPattern.edges.toSeq, Map.empty)
  }

  override def edges: Set[HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)


  /* --- Object Impl. --- */

  override def toString: String = f"VocabularyHyperGraph($edges)"


  /* --- Private Methods --- */

  private def wordToHyperEdge(word: Seq[Either[Node, EdgeType]]): HyperEdge[Node, EdgeType] = {
    def toNode(either: Either[Node, EdgeType]): Node = either match { case Left(node) => node }
    def toEdge(either: Either[Node, EdgeType]): EdgeType = either match { case Right(edge) => edge }
    HyperEdge(toNode(word(1)), toEdge(word.head), word.drop(2) map toNode)
  }

  private def hyperEdgeToWord(hyperEdge: HyperEdge[Node, EdgeType]): Seq[Either[Node, EdgeType]] =
    Right(hyperEdge.edgeType) +: (hyperEdge.target +: hyperEdge.sources).map(Left(_))
}

object VocabularyHyperGraph {
  def empty[Node, EdgeType]: VocabularyHyperGraph[Node, EdgeType] = new VocabularyHyperGraph(Vocabulary.empty)

  def apply[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): VocabularyHyperGraph[Node, EdgeType] = empty.addEdges(edges)
}
