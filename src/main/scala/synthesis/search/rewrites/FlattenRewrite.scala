package synthesis.search.rewrites

import structures._
import synthesis.search.rewrites.Template.TemplateTerm
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Language

/** The flatten works on 2 things. If we have a leftmost apply under an apply it means the function that we will use is
  * under the lower apply. We should flatten the applies to a single one. Secondly, if we have an apply which has a
  * first parameter that isn't an apply, the function that will be used, so we should flatten it into a single function.
  */
object FlattenRewrite extends RewriteRule {

  object FlattenMetadata extends Metadata {
    override protected def toStr: String = "FlattenMetadata"
  }

  private val outerApply =
    rewrites.patternEdgeCreator(Hole(0), Language.applyId, Seq(Hole(1), Repetition.rep0(Int.MaxValue, Stream.from(3, 2).map(Hole.apply)).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    rewrites.patternEdgeCreator(Hole(1), Hole(2), Seq(Repetition.rep0(Int.MaxValue, Stream.from(4, 2).map(Hole.apply)).get))

  private val applyFuncGraph: generic.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int] =
    generic.HyperGraph(Seq(outerApply, innerFunc): _*)

  //  private def getApplyMaps(state: RewriteSearchState, versioned: Boolean): Set[(Map[Int, HyperTermId], Map[Int, HyperTermIdentifier])] = {
  //    val applyEdges = state.graph.findByEdgeType(HyperTermIdentifier(Language.applyId))
  //    val appliedOn = applyEdges.map(e => (e.target, state.graph.findByTarget(e.target))).toMap
  //    for(aEdge <- applyEdges; innerEdge <- appliedOn(aEdge.sources.head) if !innerEdge.edgeType.identifier.literal.toLowerCase.contains("anchor")) yield {
  //      HyperEdge[HyperTermId, HyperTermIdentifier](aEdge.target,
  //        innerEdge.edgeType,
  //        (innerEdge.sources ++ aEdge.sources.drop(1)).toList,
  //        FlattenMetadata)
  //    }
  //  }

  override def apply(graph: RewriteRule.HyperGraph): Unit = {
    // Change apply to function
    val newFuncEdges = getNewEdges(graph, false)
    graph ++= newFuncEdges
  }

  private def getNewEdges(graph: RewriteRule.HyperGraph, versioned: Boolean) = {
    val applyEdges = graph.findByEdgeType(HyperTermIdentifier(Language.applyId))
    val appliedOn = applyEdges.map(e => (e.sources.head, graph.findByTarget(e.sources.head))).toMap
    val versionCheck =
      if (versioned) (e1: HyperEdge[HyperTermId, HyperTermIdentifier], e2: HyperEdge[HyperTermId, HyperTermIdentifier]) => (graph.isLatest(e1) || graph.isLatest(e2))
      else (e1: HyperEdge[HyperTermId, HyperTermIdentifier], e2: HyperEdge[HyperTermId, HyperTermIdentifier]) => true
    for (aEdge <- applyEdges if appliedOn.contains(aEdge.sources.head); innerEdge <- appliedOn(aEdge.sources.head) if (!innerEdge.edgeType.identifier.literal.toLowerCase.contains("anchor")) && versionCheck(aEdge, innerEdge)) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](aEdge.target,
        innerEdge.edgeType,
        (innerEdge.sources ++ aEdge.sources.drop(1)).toList,
        FlattenMetadata)
    }
  }

  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param graph     current state from which to do the initial calculations and create an operator
    * @param versioned if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(graph: RewriteRule.HyperGraph, versioned: Boolean): Set[HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]]] = {
    val newFuncEdges = getNewEdges(graph, versioned)
    newFuncEdges.map(e => e.copy(target = Explicit(e.target), edgeType = Explicit(e.edgeType), sources = e.sources.map(Explicit(_))))
  }

  /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
    * unless operator is existential
    *
    * @param graph state on which to run operator
    * @return (new state after update, next relevant version)
    */
  override def applyVersioned(graph: RewriteRule.HyperGraph): Unit = {
    val newFuncEdges = getNewEdges(graph, true)
    graph ++= newFuncEdges
  }

  override def isExistential: Boolean = false
}
