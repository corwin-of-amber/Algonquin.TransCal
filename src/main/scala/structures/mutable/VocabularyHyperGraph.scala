package structures.mutable

import com.typesafe.scalalogging.LazyLogging
import structures.HyperGraphLike.{HyperEdgeMarker, Marker}
import structures._
import structures.mutable.VocabularyHyperGraph.MarkerManager

import scala.collection.mutable

/**
  * @author tomer
  * @since 11/15/18
  */
class VocabularyHyperGraph[Node, EdgeType] private(vocabulary: Vocabulary[Either[Node, EdgeType]],
                                                   metadatas: mutable.Map[(Node, EdgeType, Seq[Node]), Metadata])
  extends generic.VocabularyHyperGraphLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]] with HyperGraph[Node, EdgeType]
    with HyperGraphLike[Node, EdgeType, VocabularyHyperGraph[Node, EdgeType]]
    with LazyLogging {

  override protected def getVocabulary: structures.Vocabulary[Either[Node, EdgeType]] = vocabulary

   override protected def getMetadata(n: Node, et: EdgeType, sources: Seq[Node]): Metadata = metadatas((n, et, sources))

  override def empty: VocabularyHyperGraph[Node, EdgeType] = VocabularyHyperGraph.empty


  val nodeMarkers: MarkerManager[Node] = new MarkerManager[Node]
  val edgeTypeMarkers: MarkerManager[EdgeType]= new MarkerManager[EdgeType]

  def this(edges: Set[HyperEdge[Node, EdgeType]] = Set.empty[HyperEdge[Node, EdgeType]]) =
    this(
      Trie(edges.map(generic.VocabularyHyperGraphLike.hyperEdgeToWord[Node, EdgeType])),
      mutable.Map(edges.map(edge => ((edge.target, edge.edgeType, edge.sources), edge.metadata)).toSeq: _*)
    )

  /* --- HyperGraphManyWithOrderToOne Impl. --- */

  override def clone: VocabularyHyperGraph[Node, EdgeType] = {
    val res = new VocabularyHyperGraph(vocabulary.clone, metadatas.clone())
    edgeTypeMarkers.copyToOther(res.edgeTypeMarkers)
    nodeMarkers.copyToOther(res.nodeMarkers)
    res
  }

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
    vocabulary replaceInPlace (Left(keep), Left(change))
    this
  }

  override def mergeEdgeTypesInPlace(keep: EdgeType, change: EdgeType): VocabularyHyperGraph[Node, EdgeType] = {
    logger.trace("Merge edge types")
    if (keep == change) return this

    val keys = metadatas.keys.filter({case (_ , et, _) => et == change})
    for ((target, edgeType, sources) <- keys) {
      val newKey = (target, keep, sources)
      metadatas(newKey) = metadatas.getOrElse(newKey, EmptyMetadata).merge(metadatas((target, edgeType, sources)))
      metadatas.remove((target, edgeType, sources))
    }

    vocabulary replaceInPlace (Right(keep), Right(change))
    this
  }

  override def edges: Set[HyperEdge[Node, EdgeType]] = vocabulary.words.map(wordToHyperEdge)

  /* --- Object Impl. --- */

  override def toString: String = f"VocabularyHyperGraph($edges)"


  /* --- IterableLike Impl. --- */
  override def copyBuilder: mutable.Builder[HyperEdge[Node, EdgeType], VocabularyHyperGraph[Node, EdgeType]] =
  new mutable.Builder[HyperEdge[Node, EdgeType], VocabularyHyperGraph[Node, EdgeType]] {
    var graph = VocabularyHyperGraph.this.clone

    override def +=(elem: HyperEdge[Node, EdgeType]): this.type = {
      graph += elem
      this
    }

    override def result(): VocabularyHyperGraph[Node, EdgeType] = {
      val res = graph
      graph = graph.clone
      graph
    }

    override def clear(): Unit = graph = VocabularyHyperGraph.empty
  }

  override def updateMetadata(edge: HyperEdge[Node, EdgeType], metadata: Metadata): Unit = {
    val meta = metadatas((edge.target, edge.edgeType, edge.sources))
    metadatas((edge.target, edge.edgeType, edge.sources)) = meta.merge(metadata)
  }

  override def markNode(n: Node): Marker[Node] = nodeMarkers.mark(n)

  override def markEdgeType(e: EdgeType): HyperGraphLike.Marker[EdgeType] = edgeTypeMarkers.mark(e)

  override def getMarkedNode(m: HyperGraphLike.Marker[Node]): Node = nodeMarkers.find(m)

  override def getMarkedEdgeType(m: HyperGraphLike.Marker[EdgeType]): EdgeType = edgeTypeMarkers.find(m)

  override def getMarkedHyperEdge(m: HyperGraphLike.Marker[HyperEdge[Node, EdgeType]]): HyperEdge[Node, EdgeType] = m match {
    case HyperEdgeMarker(e, t, ss) =>
      val edgeType = getMarkedEdgeType(e)
      val target = getMarkedNode(t)
      val sources = ss.map(getMarkedNode)
      HyperEdge(target, edgeType, sources, metadatas((target, edgeType, sources)))
    case _ => throw new RuntimeException("Bad marker type")
  }
}

object VocabularyHyperGraph extends HyperGraphCompanion[VocabularyHyperGraph] {
  override def newBuilder[A, B]: mutable.Builder[HyperEdge[A, B], VocabularyHyperGraph[A, B]] = new mutable.ListBuffer[HyperEdge[A, B]].mapResult {
    parts => {
      new VocabularyHyperGraph(parts.toSet)
    }
  }

  private class Node[T](var parentValue: Either[Node[T], T])

  class MarkerManager[T] {
    private val toMarkers: mutable.Map[T, Marker[T]] = mutable.Map.empty
    private val fromMarkers: mutable.Map[Marker[T], Node[T]] = mutable.Map.empty

    def copyToOther(other: MarkerManager[T]): Unit = {
      val nodeCopyMap = mutable.Map.empty[Node[T], Node[T]]
      def copyNode(n: Node[T]): Node[T] = {
        if (nodeCopyMap.contains(n)) nodeCopyMap(n)
        else if(n.parentValue.isRight) {
          val res = new Node[T](n.parentValue)
          nodeCopyMap(n) = res
          res
        }
        else {
          val recN = new Node[T](Left(copyNode(n.parentValue.left.get)))
          nodeCopyMap(n.parentValue.left.get) = recN
          recN
        }
      }

      other.toMarkers ++= toMarkers
      other.fromMarkers ++= fromMarkers.map({case (k, v) => (k, copyNode(v))})
    }

    def mark(n: T): Marker[T] = toMarkers.getOrElseUpdate(n, {
      val res = new Marker[T]
      fromMarkers(res) = new Node[T](Right(n))
      res
    })

    private def innerFind(m: Marker[T]): Node[T] = {
      val toUpdate = mutable.Set.empty[Node[T]]
      var base = fromMarkers(m)
      while(base.parentValue.isLeft) {
        toUpdate.add(base)
        base = base.parentValue.left.get
      }
      for (elem <- toUpdate) elem.parentValue = Left(base)
      base
    }

    def find(m: Marker[T]): T = innerFind(m).parentValue.right.get

    def merge(t1: T, t2: T): Unit = {
      val marker1 = mark(t1)
      val marker2 = mark(t2)
      val n1 = innerFind(marker1)
      val n2 = innerFind(marker2)
      toMarkers(t2) = marker1
      n2.parentValue = Left[Node[T], T](n1)
    }
  }

  /** An empty collection of type `$Coll[A]`
    *
    * @tparam A the type of the ${coll}'s elements
    */
  override def empty[A, B]: VocabularyHyperGraph[A, B] = new VocabularyHyperGraph(Set.empty[HyperEdge[A, B]])
}
