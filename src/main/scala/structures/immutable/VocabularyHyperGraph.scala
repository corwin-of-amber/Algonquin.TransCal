package structures.immutable

import com.typesafe.scalalogging.LazyLogging
import structures._
import structures.generic.HyperGraphLikeGenericCompanion

import scala.collection.{GenTraversableOnce, mutable}

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private(vocabulary: Vocabulary[Either[Node, EdgeType]], metadatas: Map[(Node, EdgeType, Seq[Node]), Metadata])
  extends generic.VocabularyHyperGraphLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]] with collection.Set[HyperEdge[Node, EdgeType]] with immutable.HyperGraph[Node, EdgeType] with LazyLogging {

  override protected def getVocabulary: Vocabulary[Either[Node, EdgeType]] = vocabulary

  override protected def getMetadatas: collection.Map[(Node, EdgeType, Seq[Node]), Metadata] = metadatas

  override def empty: VocabularyHyperGraph[Node, EdgeType] = VocabularyHyperGraph.empty

  def this(edges: Set[HyperEdge[Node, EdgeType]] = Set.empty[HyperEdge[Node, EdgeType]]) =
    this(
      Trie(edges.map(generic.VocabularyHyperGraphLike.hyperEdgeToWord[Node, EdgeType])),
      edges.map(edge => ((edge.target, edge.edgeType, edge.sources), edge.metadata)).toMap
    )

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override lazy val edgeTypes: Set[EdgeType] = vocabulary.letters.collect({case Right(edgeType) => edgeType})
  override lazy val nodes: Set[Node] = vocabulary.letters.collect({case Left(node) => node})

  override def +(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Add edge")
    val newMetadata = metadatas.updated((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources), hyperEdge.metadata)
    new VocabularyHyperGraph(vocabulary + hyperEdgeToWord(hyperEdge), newMetadata)
  }

  override def ++(hyperEdges: GenTraversableOnce[HyperEdge[Node, EdgeType]]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Add edges")
    hyperEdges.foldLeft(this)(_ + _)
  }

  override def -(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Remove edge")
    val newMetadata = metadatas.filter(t => t._1 != (hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources))
    new VocabularyHyperGraph(vocabulary - hyperEdgeToWord(hyperEdge), newMetadata)
  }

  def updateMetadata(hyperEdge: HyperEdge[Node, EdgeType]): VocabularyHyperGraph[Node, EdgeType] = {
    if (!this.contains(hyperEdge)) this
    else {
      val newMetadatas = metadatas.updated((hyperEdge.target, hyperEdge.edgeType, hyperEdge.sources), hyperEdge.metadata)
      new VocabularyHyperGraph(vocabulary, newMetadatas)
    }
  }

  override def mergeNodes(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge nodes")
    if (keep == change) return this

    def swap(n: Node) = if (n == change) keep else n

    val newMetadatas = metadatas.groupBy({
      case (edge: (Node, EdgeType, Seq[Node]), _: Metadata) if edge._1 == change || edge._3.contains(change) => (swap(edge._1), edge._2, edge._3.map(swap))
      case t => t._1
    }).mapValues(_.values.reduce(_ merge _))
    new VocabularyHyperGraph(vocabulary replace(Left(keep), Left(change)), newMetadatas)
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    if (keep == change) return this
    val newMetadatas = metadatas.filterNot(t => t._1._2 == change) ++ metadatas.filter(t => t._1._2 == change).map({case (edge: (Node, EdgeType, Seq[Node]), toChangeMetadata: Metadata) =>
      val newKey = edge.copy(_2 = keep)
      val newMetadata = metadatas.get(newKey).map(toChangeMetadata.merge).getOrElse(toChangeMetadata)
      (newKey, newMetadata)
    })
    new VocabularyHyperGraph(vocabulary replace(Right(keep), Right(change)), newMetadatas)
  }

  override lazy val edges: Set[HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)

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
