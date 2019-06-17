package synthesis.rewrites

import structures._
import structures.immutable.HyperGraphManyWithOrderToOne
import synthesis.HyperTermIdentifier
import synthesis.rewrites.rewrites._
import synthesis.search.VersionedOperator
import transcallang.{Identifier, Language}


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
  private val trueIdHole = Hole(0)
  private val functionIdHole = Hole(1)
  private val functionTypeIdHole = Hole(2)
  private val functionIdentifierHole = Hole(3)
  private val functionReturnTypeHole = Hole(4)
  private val functionApplicationHole = Hole(5)
  private val LAARGEST_STATIC_HOLE = 6
  private val argumentTypesHoles = Repetition.rep1(Int.MaxValue, Stream.from(LAARGEST_STATIC_HOLE).filter(_ % 2 == 0).map(Hole(_))).get
  private val argumentHoles = Repetition.rep1(Int.MaxValue, Stream.from(LAARGEST_STATIC_HOLE).filter(_ % 2 == 1).map(Hole(_))).get

  // Used edges
  private val trueEdge = patternEdgeCreator(trueIdHole, Identifier("truetype"), Seq())
  private val functionTypeEdge = patternEdgeCreator(trueIdHole, Language.typeId, Seq(functionIdHole, functionTypeIdHole))
  private val functionIdEdge = patternEdgeCreator(functionIdHole, functionIdentifierHole, Seq())

  private val functionReturnTypeEdge = patternEdgeCreator(functionTypeIdHole, Language.mapTypeId, Seq(argumentTypesHoles, functionReturnTypeHole))
  private val functionApplicationEdge = patternEdgeCreator(functionApplicationHole, functionIdentifierHole, Seq(argumentHoles))

  private val funcTypeGraph = HyperGraphManyWithOrderToOne(trueEdge, functionTypeEdge, functionIdEdge, functionReturnTypeEdge, functionApplicationEdge)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    val newFuncEdges = state.graph.findSubgraphVersioned[Int](funcTypeGraph, version)
      .flatMap { case (idMap, _) =>
        val argumentsHyperEdges = for (pairs <- idMap.filterKeys(LAARGEST_STATIC_HOLE <= _).groupBy(_._1 / 2).values) yield {
          val argumentType = pairs.filterKeys(_ % 2 == 0).values.head
          val argument = pairs.filterKeys(_ % 2 == 1).values.head
          HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(argument, argumentType), ArgumentsTypeMetadata)
        }
        val returnHyperEdge = HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(idMap(functionApplicationHole.id), idMap(functionReturnTypeHole.id)), ApplyTypeMetadata)
        argumentsHyperEdges.toSet + returnHyperEdge
      }

    val newGraph = state.graph.addEdges(newFuncEdges)
    (new RewriteSearchState(newGraph), newGraph.version)
  }
}
