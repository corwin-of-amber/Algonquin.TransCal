package synthesis.rewrites

import structures._
import structures.immutable.HyperGraph
import synthesis.HyperTermIdentifier
import synthesis.rewrites.Template.{ReferenceTerm, RepetitionTerm}
import synthesis.rewrites.rewrites._
import synthesis.search.VersionedOperator
import transcallang.Language


/** This rewrite rule finds functions return types by looking on their types.
  */
object FunctionArgumentsAndReturnTypeRewrite extends VersionedOperator[RewriteSearchState] {

  object ApplyTypeMetadata extends Metadata {
    override protected def toStr: String = "ApplyTypeMetadata"
  }
  object ArgumentsTypeMetadata extends Metadata {
    override protected def toStr: String = "ArgumentsTypeMetadata"
  }

  // Used holes
  private val trueIdHole = ReferenceTerm(0)
  private val functionIdHole = ReferenceTerm(1)
  private val functionTypeIdHole = ReferenceTerm(2)
  private val functionIdentifierHole = ReferenceTerm(3)
  private val functionReturnTypeHole = ReferenceTerm(4)
  private val functionApplicationHole = ReferenceTerm(5)
  private val LARGEST_STATIC_HOLE = 6
  private val argumentTypesHoles = RepetitionTerm.rep1(Int.MaxValue, Stream.from(LARGEST_STATIC_HOLE).filter(_ % 2 == 0).map(ReferenceTerm(_)))
  private val argumentHoles = RepetitionTerm.rep1(Int.MaxValue, Stream.from(LARGEST_STATIC_HOLE).filter(_ % 2 == 1).map(ReferenceTerm(_)))

  // Used edges
  private val trueEdge = patternEdgeCreator(trueIdHole, Language.typeTrueId, Seq())
  private val functionTypeEdge = patternEdgeCreator(trueIdHole, Language.typeId, Seq(functionIdHole, functionTypeIdHole))
  private val functionIdEdge = patternEdgeCreator(functionIdHole, functionIdentifierHole, Seq())

  private val functionReturnTypeEdge = patternEdgeCreator(functionTypeIdHole, Language.mapTypeId, Seq(argumentTypesHoles, functionReturnTypeHole))
  private val functionApplicationEdge = patternEdgeCreator(functionApplicationHole, functionIdentifierHole, Seq(argumentHoles))

  private val funcTypeGraph = HyperGraph(trueEdge, functionTypeEdge, functionIdEdge, functionReturnTypeEdge, functionApplicationEdge)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    val currentVersion = state.graph.version
    val newFuncEdges = state.graph.findSubgraphVersioned[Int](funcTypeGraph, version)
      .flatMap { case (idMap, _) =>
        val argumentsHyperEdges = for (pairs <- idMap.filterKeys(LARGEST_STATIC_HOLE <= _).groupBy(_._1 / 2).values) yield {
          val argumentType = pairs.filterKeys(_ % 2 == 0).values.head
          val argument = pairs.filterKeys(_ % 2 == 1).values.head
          HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(argument, argumentType), ArgumentsTypeMetadata)
        }
        val returnHyperEdge = HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(idMap(functionApplicationHole.id), idMap(functionReturnTypeHole.id)), ApplyTypeMetadata)
        argumentsHyperEdges.toSet + returnHyperEdge
      }

    val newGraph = state.graph.++(newFuncEdges)
    (new RewriteSearchState(newGraph), currentVersion)
  }
}
