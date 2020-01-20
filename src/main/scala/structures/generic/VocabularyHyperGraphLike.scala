package structures.generic

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphLike.{HyperEdgePattern, HyperGraphPattern}
import structures.VocabularyLike.Word
import structures.{EmptyMetadata, Explicit, Hole, HyperEdge, HyperGraphLike, Ignored, Item, Metadata, RegexOrdering, Repetition, Vocabulary}

import scala.collection.mutable

trait VocabularyHyperGraphLike[Node, EdgeType, +This <: VocabularyHyperGraphLike[Node, EdgeType, This] with collection.Set[HyperEdge[Node, EdgeType]]]
  extends HyperGraph[Node, EdgeType] with HyperGraphLike[Node, EdgeType, This] with LazyLogging {

  override def empty: This = newBuilder.result()

  protected def getVocabulary: Vocabulary[Either[Node, EdgeType]]
  protected def getMetadatas: collection.immutable.Map[(Node, EdgeType, Seq[Node]), Metadata]

  protected def wordToHyperEdge(word: Seq[Either[Node, EdgeType]]): HyperEdge[Node, EdgeType] = {
    def toNode(either: Either[Node, EdgeType]): Node = either match {
      case Left(node) => node
    }

    def toEdge(either: Either[Node, EdgeType]): EdgeType = either match {
      case Right(edge) => edge
    }

    val target = toNode(word(1))
    val edgeType = toEdge(word.head)
    val sources = word.drop(2) map toNode
    HyperEdge(target, edgeType, sources, getMetadatas((target, edgeType, sources)))
  }

  protected def hyperEdgeToWord(hyperEdge: HyperEdge[Node, EdgeType]): Word[Either[Node, EdgeType]] = VocabularyHyperGraphLike.hyperEdgeToWord(hyperEdge)

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
    getVocabulary.findRegex(regexAsWord).map(a => {
      val (word, map) = a
      val hyperEdge = wordToHyperEdge(word)
      val nodeMap = map.filter(_._2.isLeft).mapValues({case Left(l) => l})
      val edgeTypeMap = map.filter(_._2.isRight).mapValues({case Right(r) => r})
      (hyperEdge, nodeMap, edgeTypeMap)
    })
  }

  def findSubgraph[Id, Pattern <: HyperGraphPattern[Node, EdgeType, Id, Pattern] with collection.Set[HyperEdgePattern[Node, EdgeType, Id]]](hyperPattern: Pattern): Set[(Map[Id, Node], Map[Id, EdgeType])] = {
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

    val countNodes = hyperPattern.nodes.foldLeft(Map.empty[Item[Node, Id], Int])((map, node) => map.+((node, map.getOrElse(node, 0))))
    val countEdgeTypes = hyperPattern.edgeTypes.foldLeft(Map.empty[Item[EdgeType, Id], Int])((map, node) => map.+((node, map.getOrElse(node, 0))))
    getReferencesMap(hyperPattern.toList.sorted(new RegexOrdering(countNodes, countEdgeTypes).reverse), Map.empty, Map.empty)
  }

  /** Create a new builder from current data. When adding an edge to builder it should update the metadatastructure and
    * update the future vocabulary result.
    *
    * @return new builder for current state of graph.
    */
  def copyBuilder: mutable.Builder[HyperEdge[Node, EdgeType], This]

  override def size: Int = getVocabulary.size
}

object VocabularyHyperGraphLike {
  def hyperEdgeToWord[Node, EdgeType](hyperEdge: HyperEdge[Node, EdgeType]): Word[Either[Node, EdgeType]] =
    Right(hyperEdge.edgeType) +: (hyperEdge.target +: hyperEdge.sources).map(Left(_))
}