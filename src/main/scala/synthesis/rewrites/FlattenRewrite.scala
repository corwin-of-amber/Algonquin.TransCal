package synthesis.rewrites

import structures._
import structures.immutable.HyperGraph
import synthesis.rewrites.rewrites._
import synthesis.search.{StepOperator, VersionedOperator}
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Language


/** The flatten works on 2 things. If we have a leftmost apply under an apply it means the function that we will use is
  * under the lower apply. We should flatten the applies to a single one. Secondly, if we have an apply which has a
  * first parameter that isn't an apply, the function that will be used, so we should flatten it into a single function.
  */
object FlattenRewrite extends VersionedOperator[RewriteSearchState] with StepOperator[RewriteSearchState] {
  object FlattenMetadata extends Metadata {
    override protected def toStr: String = "FlattenMetadata"
  }

  private val outerApply =
    patternEdgeCreator(Hole(0), Language.applyId, Seq(Hole(1), Repetition.rep0(Int.MaxValue, Stream.from(3, 2).map(Hole.apply)).get))

  private val innerFunc: HyperEdge[Item[HyperTermId, Int], Item[HyperTermIdentifier, Int]] =
    patternEdgeCreator(Hole(1), Hole(2), Seq(Repetition.rep0(Int.MaxValue, Stream.from(4, 2).map(Hole.apply)).get))

  private val applyFuncGraph: generic.HyperGraph.HyperGraphPattern[HyperTermId, HyperTermIdentifier, Int] =
    HyperGraph(Seq(outerApply, innerFunc): _*)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    // Change apply to function
    val currentVersion = state.graph.version
    val newFuncEdges = getNewEdges(state, version)

    state.graph ++= newFuncEdges
    (state, currentVersion)
  }

  private def getNewEdges(state: RewriteSearchState, version: Long) = {
    val funcResults = state.graph.findSubgraphVersioned[Int](applyFuncGraph, version)
    val newFuncEdges = (for ((idMap, identMap) <- funcResults) yield {
      HyperEdge[HyperTermId, HyperTermIdentifier](idMap(0),
        identMap(2),
        (Stream.from(4, 2).takeWhile(s => idMap.contains(s)).map(idMap.apply) ++
          Stream.from(3, 2).takeWhile(s => idMap.contains(s)).map(idMap.apply)).toList,
        FlattenMetadata)
    }).filterNot(_.edgeType.identifier.literal.toLowerCase.contains("anchor"))
    newFuncEdges
  }

  /** Create an operator that finishes the action of the step operator. This should be used as a way to hold off adding
    * edges to the graph until all calculations of a step are done.
    *
    * @param state       current state from which to do the initial calculations and create an operator
    * @param lastVersion Version to use if this is a versioned step operator
    * @return an operator to later on be applied on the state. NOTICE - some operators might need state to not change.
    */
  override def getStep(state: RewriteSearchState, lastVersion: Long): VersionedOperator[RewriteSearchState] = {
    new VersionedOperator[RewriteSearchState] {
      val currentVersion = state.graph.version
      val newFuncEdges = getNewEdges(state, lastVersion)

      /** Return state after applying operator and next relevant version to run operator (should be currentVersion + 1)
        * unless operator is existential
        *
        * @param state       state on which to run operator
        * @param lastVersion version from which to look for matchers in state
        * @return (new state after update, next relevant version)
        */
      override def apply(state: RewriteSearchState, lastVersion: Long): (RewriteSearchState, Long) = {
        state.graph ++= newFuncEdges
        (state, currentVersion)
      }
    }
  }
}
