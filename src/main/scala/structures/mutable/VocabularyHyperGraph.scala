package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.VocabularyLike.Word
import structures._
import structures.generic.HyperGraphLikeGenericCompanion

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private(vocabulary: Vocabulary[Either[Node, EdgeType]], metadatas: mutable.Map[(Node, EdgeType, Seq[Node]), Metadata])
  extends generic.VocabularyHyperGraph[Node, EdgeType] with HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]]
    with LazyLogging {

  override protected def getVocabulary: structures.Vocabulary[Either[Node, EdgeType]] = vocabulary

  override protected def getMetadatas: collection.Map[(Node, EdgeType, Seq[Node]), Metadata] = metadatas

  override def empty: VocabularyHyperGraph[Node, EdgeType] = VocabularyHyperGraph.empty

  def this(edges: Set[HyperEdge[Node, EdgeType]] = Set.empty[HyperEdge[Node, EdgeType]]) =
    this(
      Trie(edges.map(generic.VocabularyHyperGraph.hyperEdgeToWord[Node, EdgeType])),
      mutable.Map(edges.map(edge => ((edge.target, edge.edgeType, edge.sources), edge.metadata)).toSeq: _*)
    )

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def clone: VocabularyHyperGraph[Node, EdgeType] = empty ++= edges
  override def edgeTypes: Set[EdgeType] = vocabulary.letters.collect({case Right(edgeType) => edgeType})
  override def nodes: Set[Node] = vocabulary.letters.collect({case Left(node) => node})

  def +=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    logger.trace("Add edge")
    metadatas((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources)) = hyperEdge.metadata
    vocabulary.+=(hyperEdgeToWord(hyperEdge))
    this
  }

  def -=(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    logger.trace("Remove edge")
    metadatas.remove((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources))
    vocabulary.-=(hyperEdgeToWord(hyperEdge))
    this
  }

  def updateMetadata(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    if (!this.contains(hyperEdge)) this
    else {
      val newMetadatas = metadatas.updated((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources), hyperEdge.metadata)
      new VocabularyHyperGraph(vocabulary, newMetadatas)
    }
  }

  def updateMetadataInPlace(hyperEdge: HyperEdge[Node, EdgeType]): this.type = {
    if (this.contains(hyperEdge))
      metadatas((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources)) = hyperEdge.metadata

    this
  }

  override def mergeNodesInPlace(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge nodes")
    if (keep == change) return this

    def swap(n: Node) = if (n == change) keep else n

    val keys = metadatas.keys.filter({case (target, _, sources) => target == change || sources.contains(change)})
    for ((target, edgeType, sources) <- keys) {
      val newKey = (swap(target), edgeType, sources.map(swap))
      metadatas(newKey) = metadatas.getOrElse(newKey, EmptyMetadata).merge(metadatas((target, edgeType, sources)))
      metadatas.remove((target, edgeType, sources))
    }

    vocabulary replace (Left(keep), Left(change))
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    if (keep == change) return this

    val keys = metadatas.keys.filter({case (target, _, sources) => target == change || sources.contains(change)})
    for ((target, edgeType, sources) <- keys) {
      val newKey = (target, keep, sources)
      metadatas(newKey) = metadatas.getOrElse(newKey, EmptyMetadata).merge(metadatas((target, edgeType, sources)))
      metadatas.remove((target, edgeType, sources))
    }

    vocabulary replace(Right(keep), Right(change))
    this
  }

  override def edges: Set[HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)

  /* --- Object Impl. --- */

  override def toString: String = f"VocabularyHyperGraph($edges)"


  /* --- IterableLike Impl. --- */

  override def newBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VocabularyHyperGraph[Node, EdgeType]] =
    new mutable.ListBuffer[HyperEdge[Node, EdgeType]].mapResult {
      parts => {
        new VocabularyHyperGraph(parts.toSet)
      }
    }
}

object VocabularyHyperGraph extends HyperGraphLikeGenericCompanion[VocabularyHyperGraph] {
  /** The default builder for `$Coll` objects.
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VocabularyHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new VocabularyHyperGraph(parts.toSet)
    }
  }
}
