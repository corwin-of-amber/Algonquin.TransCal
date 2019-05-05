package synthesis.rewrites

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import synthesis.Programs
import transcallang.{Language, TranscalParser}

/**
  * @author tomer
  * @since 2/18/19
  */
class FunctionReturnTypeRewritePropSpec extends FunSuite with Checkers {
  test("Finds in the graph") {
    val term = new TranscalParser().apply("cl = (concat: list :> list :> list) al bl")
    val programs = Programs(term)
    val graph = programs.hyperGraph

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    actualNewEdges.size == 1 & actualNewEdges.
      forall(hyperEdge => hyperEdge.edgeType.identifier == Language.typeId & hyperEdge.metadata.iterator.contains(FunctionReturnTypeRewrite.ApplyTypeMetadata))
  }

  test("Finds in the graph number 2") {
    val term = new TranscalParser().apply("bl = (flatten: list list :> list) al")
    val programs = Programs(term)
    val graph = programs.hyperGraph

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    actualNewEdges.size == 1 & actualNewEdges.
      forall(hyperEdge => hyperEdge.edgeType.identifier == Language.typeId & hyperEdge.metadata.iterator.contains(FunctionReturnTypeRewrite.ApplyTypeMetadata))
  }
}
