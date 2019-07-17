package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}
import structures.{HyperGraphLikeGenericCompanion, _}
import structures.VocabularyLike.Word

import scala.collection.{GenTraversableOnce, immutable, mutable}

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private(vocabulary: Vocabulary[Either[Node, EdgeType]], metadatas: Map[(Node, EdgeType, Seq[Node]), Metadata])
  extends HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]] with LazyLogging {

  override def empty: VocabularyHyperGraph[Node, EdgeType] = VocabularyHyperGraph.empty

  def this(edges: Set[HyperEdge[Node, EdgeType]] = Set.empty[HyperEdge[Node, EdgeType]]) =
    this(
      Trie(edges.map(VocabularyHyperGraph.hyperEdgeToWord[Node, EdgeType])),
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

  override def mergeNodes(keep: Node, change: Node): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge nodes")
    if (keep == change) return this

    def swap(n: Node) = if (n == change) keep else n

    val newMetadatas = metadatas.groupBy({
      case (edge: (Node, EdgeType, Seq[Node]), metadata: Metadata) if edge._1 == change || edge._3.contains(change) => (swap(edge._1), edge._2, edge._3.map(swap))
      case t => t._1
    }).mapValues(_.values.reduce(_ merge _))
    new VocabularyHyperGraph(vocabulary replace(Left(keep), Left(change)), newMetadatas)
  }

  override def mergeEdgeTypes(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    if (keep == change) return this
    val newMetadatas = metadatas.filterNot(t => t._1._2 == change) ++ metadatas.filter(t => t._1._2 == change).map({case (edge: (Node, EdgeType, Seq[Node]), toChangeMetadata: Metadata) => {
      val newKey = edge.copy(_2 = keep)
      val newMetadata = metadatas.get(newKey).map(toChangeMetadata.merge).getOrElse(toChangeMetadata)
      (newKey, newMetadata)
    }})
    new VocabularyHyperGraph(vocabulary replace(Right(keep), Right(change)), newMetadatas)
  }

  override def findRegex[Id](pattern: HyperEdgePattern[Node, EdgeType, Id]): Set[(HyperEdge[Node, EdgeType], Map[Id, Node], Map[Id, EdgeType])] = {
    logger.trace("Find prefix")

    def convertNode(item: Item[Node, Id]): Item[Either[Node, EdgeType], Id] = {
      item match {
        case Explicit(value) => Explicit(Left(value))
        case Hole(id) => Hole(id)
        case Ignored() => Ignored()
        case Repetition(minR, maxR, rep) => Repetition.rep(minR, maxR, rep.map(convertNode)).get
      }
    }

    def convertEdgeType(item: Item[EdgeType, Id]): Item[Either[Node, EdgeType], Id] = {
      item match {
        case Explicit(value) => Explicit(Right(value))
        case Hole(id) => Hole(id)
        case Ignored() => Ignored()
        case Repetition(minR, maxR, rep) => Repetition.rep(minR, maxR, rep.map(convertEdgeType)).get
      }
    }

    val regexAsWord = convertEdgeType(pattern.edgeType) +: (pattern.target +: pattern.sources).map(convertNode)
    vocabulary.findRegex(regexAsWord).map(a => {
      val (word, map) = a
      val hyperEdge = wordToHyperEdge(word)
      val nodeMap = map.filter(_._2.isLeft).mapValues({case Left(l) => l})
      val edgeTypeMap = map.filter(_._2.isRight).mapValues({case Right(r) => r})
      (hyperEdge, nodeMap, edgeTypeMap)
    })
  }

  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with immutable.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
    logger.trace("Find subgraph")
    type SubRegex = HyperEdgePattern[Node, EdgeType, Id]
    type ReferencesMap = (Map[Id, Node], Map[Id, EdgeType])

    /** Fills the pattern with known references.
      *
      * @param pattern       The pattern to fill
      * @param referencesNodeMap The known nodes map
      * @param referencesEdgeTypeMap The known edgeTypes map
      * @return A filled pattern
      */
    def fillReferences(pattern: SubRegex, referencesNodeMap: Map[Id, Node], referencesEdgeTypeMap: Map[Id, EdgeType]): SubRegex = {
      def convert[A](b: Map[Id, A], item: Item[A, Id]): Item[A, Id] = {
        item match {
          case Hole(id) => b.get(id).map(Explicit[A, Id]).getOrElse(item)
          case _ => item
        }
      }

      val newTarget = convert(referencesNodeMap, pattern.target)
      val newEdgeType = convert(referencesEdgeTypeMap, pattern.edgeType)
      val newSources = pattern.sources.map(convert(referencesNodeMap, _))
      val metadata = EmptyMetadata
      HyperEdge(newTarget, newEdgeType, newSources, metadata)
    }

    /** Creating reference map for a lot of matches.
      *
      * @param itemEdges     pattern edges.
      * @param referencesNodeMap current reference map of nodes
      * @param referencesEdgeTypeMap current reference map of edge types
      * @return a nee reference map
      */
    def getReferencesMap(itemEdges: Seq[SubRegex], referencesNodeMap: Map[Id, Node], referencesEdgeTypeMap: Map[Id, EdgeType]): Set[ReferencesMap] = {
      itemEdges match {
        case Nil => Set((referencesNodeMap, referencesEdgeTypeMap))
        case itemEdge +: left =>
          val filledEdge = fillReferences(itemEdge, referencesNodeMap, referencesEdgeTypeMap)
          (for ((nodeMap, edgeTypeMap) <- findRegexMaps(filledEdge)) yield {
            getReferencesMap(left, nodeMap ++ referencesNodeMap, edgeTypeMap ++ referencesEdgeTypeMap)
          }).flatten
      }
    }

    getReferencesMap(hyperPattern.toSeq, Map.empty, Map.empty)
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

  /* --- Private Methods --- */

  private def wordToHyperEdge(word: Seq[Either[Node, EdgeType]]): HyperEdge[Node, EdgeType] = {
    def toNode(either: Either[Node, EdgeType]): Node = either match {
      case Left(node) => node
    }

    def toEdge(either: Either[Node, EdgeType]): EdgeType = either match {
      case Right(edge) => edge
    }

    val target = toNode(word(1))
    val edgeType = toEdge(word.head)
    val sources = word.drop(2) map toNode
    HyperEdge(target, edgeType, sources, metadatas((target, edgeType, sources)))
  }

  private def hyperEdgeToWord(hyperEdge: HyperEdge[Node, EdgeType]): Word[Either[Node, EdgeType]] =
    VocabularyHyperGraph.hyperEdgeToWord[Node, EdgeType](hyperEdge)
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

  /* --- Private Functions --- */

  private def hyperEdgeToWord[Node, EdgeType](hyperEdge: HyperEdge[Node, EdgeType]): Word[Either[Node, EdgeType]] =
    Right(hyperEdge.edgeType) +: (hyperEdge.target +: hyperEdge.sources).map(Left(_))
}
