package synthesis.search.rewrite.operators

import org.scalatest.{Matchers, PropSpec}
import synthesis.Programs
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.FunctionArgumentsAndReturnTypeRewrite.{ApplyTypeMetadata, ArgumentsTypeMetadata}
import transcallang.{Language, TranscalParser}

/**
  * @author tomer
  * @since 2/18/19
  */
class FunctionArgumentsAndReturnTypeRewriteTest extends PropSpec with Matchers {
  property("Finds in the graph") {
    val term = new TranscalParser().apply("cl = (concat: list :> list :> list) al bl")
    val programs = Programs(term)
    val graph = programs.hyperGraph

    val newGraph = FunctionArgumentsAndReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    actualNewEdges should have size 3
    actualNewEdges.forall(hyperEdge => hyperEdge.edgeType.identifier == Language.typeId & hyperEdge.metadata.iterator.exists(Seq(ApplyTypeMetadata, ArgumentsTypeMetadata).contains(_))) shouldBe true
  }

  property("Finds in the graph number 2") {
    val term = new TranscalParser().apply("bl = (flatten: (list list) :> list) al")
    val programs = Programs(term)
    val graph = programs.hyperGraph

    val newGraph = FunctionArgumentsAndReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    actualNewEdges should have size 2
    actualNewEdges.forall(hyperEdge => hyperEdge.edgeType.identifier == Language.typeId & hyperEdge.metadata.iterator.exists(Seq(ApplyTypeMetadata, ArgumentsTypeMetadata).contains(_))) shouldBe true
  }
}
