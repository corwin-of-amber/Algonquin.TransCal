package synthesis.search.action.operators

import structures.{HyperEdge, IdMetadata, Uid}
import synthesis.search.action.operators.ObservationalEquivalence.{anchorStart, createAnchor}
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.search.rewrite.RewriteSearchState
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
class CaseSplitAction(splitterChooser: Option[CaseSplitAction.SplitChooser],
                      splitDepthOption: Option[Int],
                      maxDepthOption: Option[Int],
                      preProcessDepth: Option[Int]=None,
                      startVersioned: Boolean=false) extends Action {
  private val splitDepth = splitDepthOption.getOrElse(1)
  private val maxDepth = maxDepthOption.getOrElse(4)
  private val chooser = splitterChooser.getOrElse(CaseSplitAction.randomChooser(maxDepth, splitDepth))
  private val preProcessor = new OperatorRunAction(preProcessDepth.getOrElse(2), startVersioned=startVersioned)

  def this(splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]],
           maxDepthOption: Option[Int]) =
    this(Some(CaseSplitAction.specificChooser(splitters)), Some(splitters.length), maxDepthOption, None)

  def this(splitter: HyperEdge[HyperTermId, HyperTermIdentifier],
           maxDepthOption: Option[Int]) =
    this(Seq(splitter), maxDepthOption)

  val equivRun = new OperatorRunAction(maxDepth)
  val caseSplitPrefix = "Case_Depth_"

  def getFoundConclusionsFromRewriteState(state: RewriteSearchState, rules: Set[Operator[RewriteSearchState]])
  : Set[Set[HyperTermId]] = {
    val anchors = state.graph.nodes.map(n => ObservationalEquivalence.createAnchor(caseSplitPrefix, n))
    state.graph ++= anchors
    val newState = innerGetFoundConclusionsFromRewriteState(state, rules, Seq.empty)
    val res = ObservationalEquivalence.getIdsToMerge(caseSplitPrefix, newState.graph.edges)
    anchors.foreach(a => {state.graph --= state.graph.findByEdgeType(a.edgeType)})
    res
  }

  private def innerGetFoundConclusionsFromRewriteState(state: RewriteSearchState,
                                                       rules: Set[Operator[RewriteSearchState]],
                                                       chosen: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : RewriteSearchState = {
    val newState = preProcessor.fromRewriteState(state, rules)
    val splitters = chooser(state, chosen).toSeq
    if (chosen.length >= splitDepth || splitters.isEmpty) {
      equivRun.fromRewriteState(state, rules)
    } else {
      val currentPrefix = caseSplitPrefix + chosen.length.toString + "_"
      val withAnchors = newState.graph ++ newState.graph.nodes.map(ObservationalEquivalence.createAnchor(currentPrefix, _))
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
        val tempGraph = withAnchors.clone
        tempGraph -= splitter
        val results = targets.par.map(t => {
          // 1 + 2. pre run ops
          val newState = RewriteSearchState(tempGraph.mergeNodes(source, t))
          // 3. Recursion
          innerGetFoundConclusionsFromRewriteState(newState, rules, chosen :+ splitter)
        }).seq
        // 3b. Merge recursion results

        ObservationalEquivalence.flattenIntersectConclusions(results.map(r =>
          ObservationalEquivalence.getIdsToMerge(currentPrefix, r.graph.edges)))
      })
      // 4. Merge different split results
      ObservationalEquivalence.mergeConclusions(state, ObservationalEquivalence.flattenUnionConclusions(equives).toSeq)
    }
  }

  def getFoundConclusions(state: ActionSearchState): Set[Set[HyperTermId]] = {
    getFoundConclusionsFromRewriteState(new RewriteSearchState(state.programs.hyperGraph), state.rewriteRules)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    val rState = new RewriteSearchState(state.programs.hyperGraph)
    val toMerge = getFoundConclusionsFromRewriteState(rState, state.rewriteRules).toSeq
    val newState = ObservationalEquivalence.mergeConclusions(rState, toMerge)
    ActionSearchState(Programs(newState.graph), state.rewriteRules)
  }
}

object CaseSplitAction {
  val possibleSplitId = Identifier("possibleSplit")
  val splitTrue = Identifier("splitTrue")
  type SplitChooser = (RewriteSearchState, Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) =>
    Set[HyperEdge[HyperTermId, HyperTermIdentifier]]

  def specificChooser(splitters: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]): SplitChooser = {
    (state: RewriteSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      if (chose.length == splitters.length) Set.empty
      else splitters.take(chose.length + 1).toSet
    }
  }

  // TODO: create utils class and move it there
  def shiftEdges(startId: Int, edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]])
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] =
    edges.map(e => e.copy(target = e.target.copy(e.target.id + startId),
      sources = e.sources.map(hid => hid.copy(id = hid.id + startId))))

  def disjointAppend(graphs: Seq[RewriteSearchState.HyperGraph]): RewriteSearchState.HyperGraph = {
    graphs.fold(new RewriteSearchState.HyperGraph)({
      case (g1, g2) if g1.nonEmpty => g1 ++= CaseSplitAction.shiftEdges(g1.nodes.map(_.id).max, g2.edges)
      case (_, g2) => g2
    })
  }

  def edgesMax(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]]): Int =
    edges.flatMap(e => e.sources :+ e.target).map(_.id).max

  def mergeNodes(edges: Set[HyperEdge[HyperTermId, HyperTermIdentifier]], source: HyperTermId, target: HyperTermId)
  : Set[HyperEdge[HyperTermId, HyperTermIdentifier]] = {
    def switch(hyperTermId: HyperTermId) = if (hyperTermId == target) source else hyperTermId

    edges.map(e =>
      if (e.sources.contains(target) || e.target == target)
        e.copy(target = switch(e.target), sources = e.sources.map(switch))
      else e
    )
  }

  def randomChooser(depth: Int, splitDepth: Int): SplitChooser = {
    val depthUids = (0 to splitDepth).map(_ => IdMetadata(new Uid))
    (state: RewriteSearchState, chose: Seq[HyperEdge[HyperTermId, HyperTermIdentifier]]) => {
      //      val uidsAbove = depthUids.take(chose.length)
      //      chose.zip(uidsAbove).foreach({case (e, m) =>
      //          val updated = e.copy(metadata = e.metadata.merge(m))
      //          state.graph -= updated
      //          state.graph += updated
      //      })
      // TODO: Fix API so we can save have the chosen
      if (chose.length == splitDepth) Set.empty
      else state.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId))
      //      state.graph.findByEdgeType(HyperTermIdentifier(CaseSplitAction.possibleSplitId)).filterNot(e =>
      ////        chose.contains(e) || e.metadata.exists(m => uidsAbove.contains(m)))
      //        chose.contains(e))
    }
  }
}
