package synthesis.rewrites

import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import synthesis.Programs
import transcallang.{Language, TranscalParser}

/**
  * @author tomer
  * @since 2/18/19
  */
class FunctionReturnTypeRewritePropSpec extends PropSpec with Checkers {
  property("Finds in the graph") {
    val term = new TranscalParser().apply("cl = (concat: list :> list :> list) al bl")
    val programs = Programs(term)
    val graph = programs.hyperGraph

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    check(actualNewEdges.size == 1 & actualNewEdges.head.edgeType.identifier == Language.typeId
      & actualNewEdges.head.metadata.iterator.contains(FunctionReturnTypeRewrite.ApplyTypeMetadata))
  }
}
