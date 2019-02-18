package synthesis.rewrites

import language.Language
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import syntax.AstSugar._
import structures.immutable.CompactHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 2/18/19
  */
class FunctionReturnTypeRewritePropSpec extends PropSpec with Checkers {
  property("Finds in the graph") {
    val graph = CompactHyperGraph(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(I("f")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(3), HyperTermId(4)), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(5), HyperTermId(6)), EmptyMetadata),
      HyperEdge(HyperTermId(7), HyperTermIdentifier(I("f")), Seq(HyperTermId(8), HyperTermId(9), HyperTermId(10)), EmptyMetadata)
    )
    val expectedNewEdges = Set(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(7), HyperTermId(6)), EmptyMetadata)
    )

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    check(actualNewEdges == expectedNewEdges)
  }
}
