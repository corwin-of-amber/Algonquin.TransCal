package synthesis.search.action.operators.thesy

import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.Operator
import synthesis.search.action.ActionSearchState
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.search.action.operators.{CaseSplitAction, ObservationalEquivalence, OperatorRunWithCaseSplit, SearchAction}
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.RewriteRule
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
      val example = valuations(i)
      currentGraph ++= Programs.destruct(example cleanTypes, maxId = currentGraph.nodes.maxBy(_.id))
      val (pattern, root) = Programs.destructPatternWithRoot(example cleanTypes)
      val id = currentGraph.findSubgraph[Int](pattern).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
      currentGraph.mergeNodesInPlace(currentGraph.findByEdgeType(HyperTermIdentifier(marker)).head.target, id)
      currentGraph --= currentGraph.findByEdgeType(HyperTermIdentifier(marker))
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
    val prefixes = valuations.indices.map(i => s"${
      i
    }_$anchorPrefix")
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
