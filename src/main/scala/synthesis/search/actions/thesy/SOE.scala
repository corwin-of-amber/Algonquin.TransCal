package synthesis.search.actions.thesy

import com.typesafe.scalalogging.LazyLogging
import report.LazyTiming
import structures.generic.HyperGraph
import structures.generic.HyperGraph.Match
import structures.mutable.CompactHyperGraph
import structures.{EmptyMetadata, HyperEdge, Metadata, UnionMetadata}
import synthesis.search.actions.thesy.SyGuERewriteRules.SyGuEMetadata
import synthesis.search.{ActionSearchState, Operator}
import synthesis.search.actions.{Action, ObservationalEquivalence, SearchAction}
import synthesis.search.rewrites.PatternRewriteRule.MutableHyperPattern
import synthesis.search.rewrites.RewriteRule
import synthesis.search.rewrites.Template.ReferenceTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, search}
import transcallang.{AnnotatedTree, Identifier, Language}

class SOE(searcher: SearchAction, state: ActionSearchState, inputMarkers: Seq[Identifier], valuations: Seq[Seq[AnnotatedTree]])
    extends LazyLogging with LazyTiming {
  def this(searcher: SearchAction, state: ActionSearchState, inputMarker: Identifier, valuations: Seq[AnnotatedTree]) =
    this(searcher, state, Seq(inputMarker), Seq(valuations))

  private val anchorPrefix = "SOE_"
  val markers = inputMarkers.map(_.copy(annotation = None))

  private lazy val tupeledState: ActionSearchState = timed("tupled graph creation") {
    // TODO: Add disjoint append to rewrite graph and simplify this
    // Need to shift all edges on graph because we want to prevent one changed value from affecting parts of other
    // values. Working on edges to prevent unwanted compaction until adding back anchors and adding new tuples.

    val replacedGraphs = markers.zipWithIndex.flatMap({case (marker, index) => valuations(index).indices.map(i => {
      val itAnchorPrefix = s"${valuations.take(index).map(_.size).sum + i}_$anchorPrefix"
      val currentState = state.deepCopy()
      currentState.updateGraph(currentGraph => {
      currentGraph --= currentGraph.filter(e => e.edgeType.identifier == Language.typeId
        || e.edgeType.identifier == SyGuERewriteRules.sygueCreatedId)
      // DO NOT CHANGE GRAPH BEFORE ADDING ANCHORS. SEE getTupledConclusions
      currentGraph ++= currentGraph.edges.map(e => ObservationalEquivalence.createAnchor(itAnchorPrefix, e.target))
      val example = valuations(index)(i)
      currentGraph ++= Programs.destruct(example cleanTypes, maxId = currentGraph.nodes.maxBy(_.id))
      val (pattern, root) = Programs.destructPatternWithRoot(example cleanTypes)
      val id = currentGraph.findSubgraph[Int](pattern).head.nodeMap(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
      currentGraph.mergeNodesInPlace(currentGraph.findByEdgeType(HyperTermIdentifier(marker)).head.target, id)
      currentGraph --= currentGraph.findByEdgeType(HyperTermIdentifier(marker))
//      currentGraph.foreach(e => currentGraph.updateMetadata(e, IterationMetadata(i)))
//      currentGraph
    })
      currentState
    })})
    val resState = replacedGraphs.head
    var max = resState.programs.queryGraph.nodes.maxBy(_.id)
    resState.updateGraph(resGraph => {
      replacedGraphs.tail.foreach(g => {
        resGraph ++= search.shiftEdges(max.id + 1, g.programs.queryGraph.edges)
        max = resGraph.nodes.maxBy(_.id)
      })
    })
    resState
  }

  private def getTupledConclusions(state: ActionSearchState): Set[Set[HyperTermId]] = timed {
    val prefixes = valuations.zipWithIndex.map({case (valuation, index) => valuation.indices.map(i => s"${
      valuations.take(index).map(_.size).sum + i
    }_$anchorPrefix")})
    // This can work because when we create the tupled graph we do not change the graph when inserting different anchors
    ObservationalEquivalence.flattenUnionConclusions(
      prefixes.map(prefs => ObservationalEquivalence.flattenIntersectConclusions(prefs.map( p =>
      ObservationalEquivalence.getIdsToMerge(p, state.programs.queryGraph.edges))))).filter(_.nonEmpty)
  }

  def findEquives(rules: Set[RewriteRule], depth: Option[Double]): Set[Set[HyperTermId]] = timed ("find equives") {
    // Copy of graph is needed because we do not want merges to change our anchored nodes here.
    tupeledState.addRules(rules)
    val res = searcher(tupeledState, depth)
    getTupledConclusions(res)
  }

  def updateGraph(operator: ActionSearchState.GraphUpdater): Unit = {
    tupeledState.updateGraph(operator)
  }

  // TODO: Once we have markers make this work and enable equivalence classes
//  def toSymbolicState: RewriteSearchState = {
//    // Tupeled state can be changed during work. This removed examples and merges back to original marker
//    val temp = tupeledState.deepCopy()
//    val newTarget = HyperTermId(tupeledState.graph.nodes.maxBy(_.id).id + 1)
//    temp.graph += HyperEdge(newTarget, HyperTermIdentifier(marker), Seq.empty, EmptyMetadata)
//    val patternsWithRoots = valuations.map(t => Programs.destructPatternWithRoot(t cleanTypes))
//    patternsWithRoots.foreach({
//      case (p, r) =>
//        val maps = temp.graph.findSubgraphVersioned[Int](p)
//        val exampleGraph = RewriteRule.fillPatterns(temp.graph, Seq(p)).next().head
//        temp.graph --= exampleGraph
//        temp.graph.mergeNodesInPlace(newTarget, maps.head._1(r.id))
//    })
//    temp
//  }

//  override def createClasses(param: Set[Operator[RewriteSearchState]]): EquivalenceClasses[AnnotatedTree] = {
//    new EquivalenceClasses[AnnotatedTree] {
//      val classes = {
//        val equives = findEquives(param.toSeq)
//
//      }
//
//      override def getClasses: Map[AnnotatedTree, Set[AnnotatedTree]] = classes
//    }
//  }
}

case class IterationMetadata(iteration: Int) extends Metadata {
  override protected def toStr: String = s"IterationMetadata($iteration)"
}