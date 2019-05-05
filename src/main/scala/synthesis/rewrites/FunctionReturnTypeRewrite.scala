package synthesis.rewrites

import structures._
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.HyperTermIdentifier
import synthesis.rewrites.rewrites._
import synthesis.search.VersionedOperator
import transcallang.Language


/** This rewrite rule finds functions return types by looking on their types.
  */
object FunctionReturnTypeRewrite extends VersionedOperator[RewriteSearchState] {

  object ApplyTypeMetadata extends Metadata {
    override protected def toStr: String = "ApplyTypeMetadata"
  }

  // Used holes
  private val trueIdHole = Hole(0)
  private val functionIdHole = Hole(1)
  private val functionTypeIdHole = Hole(2)
  private val functionIdentifierHole = Hole(3)
  private val functionReturnTypeHole = Hole(4)
  private val functionApplicationHole = Hole(5)

  // Used edges
  private val trueEdge = patternEdgeCreator(trueIdHole, Language.trueId, Seq())
  private val functionTypeEdge = patternEdgeCreator(trueIdHole, Language.typeId, Seq(functionIdHole, functionTypeIdHole))
  private val functionIdEdge = patternEdgeCreator(functionIdHole, functionIdentifierHole, Seq())

  private val functionReturnTypeEdge = patternEdgeCreator(functionTypeIdHole, Language.mapTypeId, Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, functionReturnTypeHole))
  private val functionApplicationEdge = patternEdgeCreator(functionApplicationHole, functionIdentifierHole, Seq(Repetition.rep0(Int.MaxValue, Ignored()).get))

  private val funcTypeGraph = HyperGraphManyWithOrderToOne(trueEdge, functionTypeEdge, functionIdEdge, functionReturnTypeEdge, functionApplicationEdge)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    val newFuncEdges = for (
      (idMap, _) <- state.graph.findSubgraphVersioned[Int](funcTypeGraph, version)
    ) yield {
        HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(idMap(functionApplicationHole.id), idMap(functionReturnTypeHole.id)), ApplyTypeMetadata)
    }

    val newGraph = state.graph.addEdges(newFuncEdges)
    (new RewriteSearchState(newGraph), newGraph.version)
  }
}
