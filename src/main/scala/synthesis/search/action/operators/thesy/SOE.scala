package synthesis.search.action.operators.thesy

import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.search.action.operators.{CaseSplitAction, ObservationalEquivalence, OperatorRunWithCaseSplit, SearchAction}
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.Template.ReferenceTerm
import transcallang.{AnnotatedTree, Identifier}

class SOE(searcher: SearchAction, rewriteSearchState: RewriteSearchState, inputMarker: Identifier, valuations: Seq[AnnotatedTree]) {
  private val anchorPrefix = "SOE_"
  val marker = inputMarker.copy(annotation = None)

  private val tupeledState: RewriteSearchState = {
    // TODO: Add disjoint append to rewrite graph and simplify this
    // Need to shift all edges on graph because we want to prevent one changed value from affecting parts of other
    // values. Working on edges to prevent unwanted compaction until adding back anchors and adding new tuples.
    val replacedGraphs = valuations.indices.map(i => {
      val itAnchorPrefix = s"${i}_$anchorPrefix"
      //      val currentGraph = rewriteSearchState.graph.clone.filterNot(e => e.edgeType.identifier == Language.typeId
      //        || e.edgeType.identifier == SyGuSRewriteRules.sygusCreatedId)
      val currentGraph = rewriteSearchState.graph.clone
      // DO NOT CHANGE GRAPH BEFORE ADDING ANCHORS. SEE getTupledConclusions
      currentGraph ++= currentGraph.edges.map(e => ObservationalEquivalence.createAnchor(itAnchorPrefix, e.target))
      valuations.foreach(example => {
        currentGraph ++= Programs.destruct(example cleanTypes, maxId = currentGraph.nodes.maxBy(_.id))
        val (pattern, root) = Programs.destructPatternsWithRoots(Seq(example cleanTypes)).head
        val id = currentGraph.findSubgraph[Int](pattern).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
        currentGraph.mergeNodesInPlace(currentGraph.findByEdgeType(HyperTermIdentifier(marker)).head.target, id)
        // TODO: cant remove marker and also use marker for finding this later. Add iter to marker?
        currentGraph --= currentGraph.findByEdgeType(HyperTermIdentifier(marker))
    })
      currentGraph
    })
    val resGraph = replacedGraphs.head
    var max = resGraph.nodes.maxBy(_.id)
    replacedGraphs.tail.foreach(g => {
      resGraph ++= CaseSplitAction.shiftEdges(max.id + 1, g.edges)
      max = resGraph.nodes.maxBy(_.id)
    })
    new RewriteSearchState(resGraph)
  }

  private def getTupledConclusions(rewriteSearchState: RewriteSearchState): Set[Set[HyperTermId]] = {
    val prefixes = valuations.indices.map(i => s"${i}_$anchorPrefix")
    // This can work because when we create the tupled graph we do not change the graph when inserting different anchors
    ObservationalEquivalence.flattenIntersectConclusions(prefixes.map(p =>
      ObservationalEquivalence.getIdsToMerge(p, rewriteSearchState.graph.edges))).filter(_.nonEmpty)
  }

  def findEquives(rules: Seq[Operator[RewriteSearchState]]): Set[Set[HyperTermId]] = {
    // Copy of graph is needed because we do not want merges to change our anchored nodes here.
    val res = searcher.fromRewriteState(tupeledState, rules.toSet)
    getTupledConclusions(res)
  }

  def updateGraph(operator: Operator[RewriteSearchState]): Unit = {
    operator(tupeledState)
  }
}
