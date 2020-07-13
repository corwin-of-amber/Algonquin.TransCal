package synthesis.search.actions

import structures.{HyperEdge, IdMetadata, Uid}
import ObservationalEquivalence.{anchorStart, createAnchor}
import synthesis.search.{ActionSearchState, Operator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.Identifier


/** Case split action splits a variable into options specified by the splitter edge. Variable edges are deleted to
  * prevent merging of possible values.
  * A new graph is created containing tuples of each expression in the original graph where each tuple is the
  * expression with the value of the variable changed to the appropriate value.
  * Than symbolic  observational equivalence will be run and the results are than matched and nodes that should merge
  * will be merged in the result state.
  *
  * Input graph should contain an edge of type possibleSplit where the target is SplitTrue and the first source is the
  * variable to change and possible values are the rest of the sources.
  */
class CaseSplitAction(searcher: SearchAction,
                      preprocessorOption: Option[SearchAction],
                      splitterChooser: Option[CaseSplitAction.SplitChooser],
                      splitDepthOption: Option[Int]) extends SearchAction {
  private val splitDepth = splitDepthOption.getOrElse(1)
  private val chooser = splitterChooser.getOrElse(CaseSplitAction.randomChooser(splitDepth))

  def this(searchAction: SearchAction) = this(searchAction, None, None, None)

  def this(searchAction: SearchAction, preprocessor: SearchAction) = this(searchAction, Some(preprocessor), None, None)

  def this(searchAction: SearchAction, splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) =
    this(searchAction, None, splitterChooser = Some(CaseSplitAction.specificChooser(splitters)), splitDepthOption = Some(splitters.length))

  def this(searchAction: SearchAction, splitter: HyperEdge[HyperTermId, HyperTermIdentifier]) =
    this(searchAction, Seq(splitter))

  val caseSplitPrefix = "Case_Depth_"

  protected val preprocessor: SearchAction = preprocessorOption.getOrElse(searcher)

  private def innerGetFoundConclusions(state: ActionSearchState,
                                       chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]],
                                       depth: Option[Double], preprocessorDepth: Option[Double])
  : ActionSearchState = {
    val newState = preprocessor(state, preprocessorDepth)
    val splitters = chooser(newState, chosen).toSeq
    if (splitters.isEmpty) {
      searcher(newState, depth.map(_ - preprocessorDepth.getOrElse(depth.get/2)))
    } else if(chosen.length >= splitDepth) {
      searcher(newState, depth)
    } else {
      val currentPrefix = caseSplitPrefix + chosen.length.toString + "_"
      // TODO: remove this deep copy if possible
      val withAnchors = newState.deepCopy()
      withAnchors.updateGraph(graph => graph ++= graph.nodes.map(ObservationalEquivalence.createAnchor(currentPrefix, _)))
      // For each splitter edge:
      // 1. build new graphs
      // 2. pre run ops when
      // 3. enter recursively
      // TODO: 3a. early merge results into graph to help next steps
      // 3b. merge recursive results
      // 4. merge all the results
      // Note: Don't keep the graphs after your finished, but don't mess up previous graph
      val equives = splitters.zipWithIndex.map({ case (splitter, i) =>
        // 1. build new graph - for each possible value copy graph and merge the needed value
        val source = splitter.sources.head
        val targets = splitter.sources.tail
        val results = targets.par.map(t => {
          // 1 + 2. pre run ops
          val tempState = withAnchors.deepCopy()
          tempState.updateGraph(g => g -= splitter)
          tempState.updateGraph(g => g.mergeNodesInPlace(source, t))
          // 3. Recursion
          innerGetFoundConclusions(tempState, chosen :+ splitter, depth, preprocessorDepth)
        }).seq
        // 3b. Merge recursion results

        ObservationalEquivalence.flattenIntersectConclusions(results.map(r =>
          ObservationalEquivalence.getIdsToMerge(currentPrefix, r.programs.queryGraph.edges)))
      })
      // 4. Merge different split results
      ObservationalEquivalence.mergeConclusions(state, ObservationalEquivalence.flattenUnionConclusions(equives).toSeq)
    }
  }

  def getFoundConclusions(state: ActionSearchState, depth: Option[Double], preprocessorDepth: Option[Double]=None): Set[Set[HyperTermId]] = {
    val anchors = state.programs.queryGraph.nodes.map(n => ObservationalEquivalence.createAnchor(caseSplitPrefix, n))
    state.updateGraph(graph => {
      graph ++= anchors
    })
    val newState = innerGetFoundConclusions(state, Seq.empty, depth, preprocessorDepth)
    val res = ObservationalEquivalence.getIdsToMerge(caseSplitPrefix, newState.programs.queryGraph.edges)
    newState.updateGraph(graph => anchors.foreach(a => {
      graph --= graph.findByEdgeType(a.edgeType)
    }))
    res
  }

  override def apply(state: ActionSearchState, depth: Option[Double]): ActionSearchState = {
    val toMerge = getFoundConclusions(state, depth, depth.map(d => (d/2).ceil)).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(state, toMerge)
    newState
  }

  def apply(state: ActionSearchState, depth: Double, preprocessorDepth: Double): ActionSearchState = {
    val toMerge = getFoundConclusions(state, Some(depth), Some(preprocessorDepth)).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(state, toMerge)
    newState
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
  type SplitChooser = (ActionSearchState, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
    Set[HyperEdge[HyperTermId, HyperTermIdentifier]]

  def specificChooser(splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]): SplitChooser = {
    (state: ActionSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      if (chose.length == splitters.length) Set.empty
      else splitters.take(chose.length + 1).toSet
    }
  }

  def randomChooser(splitDepth: Int): SplitChooser = {
    val depthUids = (0 to splitDepth).map(_ => IdMetadata(new Uid))
    (state: ActionSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      //      val uidsAbove = depthUids.take(chose.length)
      //      chose.zip(uidsAbove).foreach({case (e, m) =>
      //          val updated = e.copy(metadata = e.metadata.merge(m))
      //          state.graph -= updated
      //          state.graph += updated
      //      })
      // TODO: Fix API so we can save have the chosen
      if (chose.length == splitDepth) Set.empty
      else state.programs.queryGraph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
      //      state.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).filterNot(e =>
      ////        chose.contains(e) || e.metadata.exists(m => uidsAbove.contains(m)))
      //        chose.contains(e))
    }
  }
}
