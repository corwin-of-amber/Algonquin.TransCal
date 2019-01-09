package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.HyperGraphManyWithOrderToOneLike._
import structures.VocabularyLike.Word

import scala.language.postfixOps

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private (vocabulary: Vocabulary[Either[Node, EdgeType]], metadatas: Map[(Node, EdgeType, Seq[Node]), Metadata])
  extends HyperGraphManyWithOrderToOne[Node, EdgeType]
    with HyperGraphManyWithOrderToOneLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]] with LazyLogging {


  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def addEdge(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Add edge")
    val newMetadata = metadatas + (((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources), hyperEdge.metadata))
    new VocabularyHyperGraph(vocabulary + hyperEdgeToWord(hyperEdge), newMetadata)
  }

  override def addEdges(hyperEdges: Set[HyperEdge[Node, EdgeType]]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Add edges")
    val newMetadata = metadatas ++ hyperEdges.map(hyperEdge=>((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources), hyperEdge.metadata))
    new VocabularyHyperGraph(vocabulary :+ hyperEdges.map(hyperEdgeToWord), newMetadata)
  }

  override def removeEdge(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Remove edge")
    val newMetadata = metadatas.filter(t=>t._1 != (hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources))
      new VocabularyHyperGraph(vocabulary - hyperEdgeToWord(hyperEdge), newMetadata)
  }

  override def mergeNodes(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge nodes")
    def swap(n: Node) = if(n == change) keep else n
    val newMetadatas = metadatas.filterNot(t=>t._1._1 == change || t._1._3.contains(change)) ++
      metadatas.filter(t=>t._1._1 == change || t._1._3.contains(change))
      .groupBy(t => (swap(t._1._1), t._1._2, t._1._3.map(swap)))
      .map(t=>{
        val a= t._2.values.foldLeft(metadatas.getOrElse(t._1, EmptyMetadata))((m1, m2)=>m1.merge(m2))
        (t._1, a)
      }
    )
    new VocabularyHyperGraph(vocabulary replace (Left(keep), Left(change)), newMetadatas)
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    val newMetadatas = metadatas.filterNot(t=>t._1._2 == change) ++ metadatas.filter(t=>t._1._2 == change).map(t=>{
      val target = t._1._1
      val sources = t._1._3
      val toChangeMetadata = t._2
      val newKey = (target, keep, sources)
      val newMetadata = metadatas.get(newKey).map(toChangeMetadata.merge).getOrElse(toChangeMetadata)
      (newKey, newMetadata)
    })
    new VocabularyHyperGraph(vocabulary replace (Right(keep), Right(change)), newMetadatas)
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

  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    logger.trace("Find subgraph")
    type SubPattern = HyperEdgePattern[Node, EdgeType, Id]
    type ReferencesMap = (Map[Id, Node], Map[Id, EdgeType])
    /** Creating a new references map from known hyper edge and pattern.
      * @param knownEdge The known edge
      * @param pattern The pattern
      * @return A map of the references in pattern to the values in knownPattern.
      */
    def hyperEdgeAndTemplateToReferencesMap(knownEdge: HyperEdge[Node, EdgeType], pattern: SubPattern): ReferencesMap = {
      val nodesRefs = (pattern.target +: pattern.sources).zip(knownEdge.target +: knownEdge.sources).filter(a=>a._1.isInstanceOf[Hole[Node, Id]]).map(a=>a._1 match {
        case Hole(id) => (id, a._2)
      })
      val edgeTypeRef = pattern.edgeType match {
        case Hole(id) => Some((id,  knownEdge.edgeType))
        case _ => None
      }
      (nodesRefs.toMap, edgeTypeRef.toMap)
    }

    /** Fills the pattern with known references.
      * @param pattern The pattern to fill
      * @param referencesMap The known map
      * @return A filled pattern
      */
    def fillReferences(pattern: SubPattern, referencesMap: ReferencesMap): SubPattern = {
      def convert[A](b: Map[Id, A], item: Item[A, Id]): Item[A, Id] = {
        item match {
          case Hole(id) => b.get(id).map(Explicit[A, Id]).getOrElse(item)
          case _ => item
        }
      }
      val newTarget = convert(referencesMap._1, pattern.target)
      val newEdgeType = convert(referencesMap._2, pattern.edgeType)
      val newSources = pattern.sources.map(convert(referencesMap._1, _))
      val metadata = EmptyMetadata
      HyperEdge(newTarget, newEdgeType, newSources, metadata)
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
            val temp = hyperEdgeAndTemplateToReferencesMap(hyperEdge, filledEdge)
            val newReferences = (temp._1 ++ referencesMap._1, temp._2 ++ referencesMap._2)
            getReferencesMap(left, newReferences)
          }).flatten
      }
    }
    getReferencesMap(hyperPattern.edges.toSeq, (Map.empty, Map.empty))
  }

  override def edges: Set[HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)

  /* --- Object Impl. --- */

  override def toString: String = f"VocabularyHyperGraph($edges)"


  /* --- Private Methods --- */

  private def wordToHyperEdge(word: Seq[Either[Node, EdgeType]]): HyperEdge[Node, EdgeType] = {
    def toNode(either: Either[Node, EdgeType]): Node = either match { case Left(node) => node }
    def toEdge(either: Either[Node, EdgeType]): EdgeType = either match { case Right(edge) => edge }
    val target = toNode(word(1))
    val edgeType = toEdge(word.head)
    val sources = word.drop(2) map toNode
    HyperEdge(target, edgeType, sources, metadatas((target, edgeType, sources)))
  }

  private def hyperEdgeToWord(hyperEdge: HyperEdge[Node, EdgeType]): Word[Either[Node, EdgeType]] =
    Right(hyperEdge.edgeType) +: (hyperEdge.target +: hyperEdge.sources).map(Left(_))

  override def foreach[U](f: HyperEdge[Node, EdgeType] => U): Unit = edges.foreach(f)
}

object VocabularyHyperGraph {
  def empty[Node, EdgeType]: VocabularyHyperGraph[Node, EdgeType] = new VocabularyHyperGraph(Vocabulary.empty, Map.empty)

  def apply[Node, EdgeType](edges: Set[HyperEdge[Node, EdgeType]]): VocabularyHyperGraph[Node, EdgeType] = empty :+ edges
}
