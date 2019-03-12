package synthesis.rewrites

import structures.HyperGraphManyWithOrderToOneLike.HyperEdgePattern
import structures._
import structures.immutable.{HyperGraphManyWithOrderToOne, VersionedHyperGraph, VocabularyHyperGraph}
import synthesis.rewrites.rewrites._
import synthesis.search.{Operator, VersionedOperator}
import synthesis.{HyperTermId, HyperTermIdentifier}
import transcallang.Language

import scala.annotation.tailrec


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

  // Used edges
  private val trueEdge = patternEdgeCreator(trueIdHole, Language.trueId, Seq())
  private val functionTypeEdge = patternEdgeCreator(trueIdHole, Language.typeId, Seq(functionIdHole, functionTypeIdHole))
  private val functionIdEdge = patternEdgeCreator(functionIdHole, functionIdentifierHole, Seq())

  @tailrec
  private def createMapTypeEdge(baseId: Item[HyperTermId, Int], holeCreator: () => Int, counter: Int, createdEdges: Set[HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]]): (Set[HyperEdgePattern[HyperTermId, HyperTermIdentifier, Int]], Hole[HyperTermId, Int]) = {
    val newBaseId = Hole(holeCreator())
    if (counter == 0) {
      (createdEdges + patternEdgeCreator(baseId, Language.mapTypeId, Seq(Ignored(), newBaseId)), newBaseId)
    } else {
      val newCreatedEdges = createdEdges + patternEdgeCreator(baseId, Language.mapTypeId, Seq(Ignored(), newBaseId))
      createMapTypeEdge(newBaseId, holeCreator, counter - 1, newCreatedEdges)
    }
  }
  private val functionReturnTypeEdge = patternEdgeCreator(functionTypeIdHole, Language.mapTypeId, Seq(Ignored(), Ignored()))

  private val funcTypeGraph = HyperGraphManyWithOrderToOne(trueEdge, functionTypeEdge, functionIdEdge, functionReturnTypeEdge)

  override def apply(state: RewriteSearchState, version: Long): (RewriteSearchState, Long) = {
    val newFuncEdges = for (
      (idMap, identMap) <- state.graph.findSubgraphVersioned[Int](funcTypeGraph, version);
      functionEdges = state.graph.findEdges(identMap(functionIdentifierHole.id)).filter(_.sources.nonEmpty);
      numberOfArguments = functionEdges.map(_.sources.size).max;
      functionEdge <- functionEdges;
      (edges, last) = createMapTypeEdge(Explicit(idMap(functionTypeIdHole.id)), Stream.from(1 + functionTypeIdHole.id).toIterator.next, numberOfArguments - 2, Set.empty);
      (typeIdMap, _) <- state.graph.findSubgraphVersioned[Int](VocabularyHyperGraph(edges.toSeq:_ *), version)
    ) yield {
      HyperEdge(idMap(trueIdHole.id), HyperTermIdentifier(Language.typeId), Seq(functionEdge.target, typeIdMap(last.id)), ApplyTypeMetadata)
    }

    val newGraph = state.graph.addEdges(newFuncEdges)
    (new RewriteSearchState(newGraph), newGraph.version)
  }
}
