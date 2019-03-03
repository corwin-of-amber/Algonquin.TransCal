package synthesis.rewrites

import language.Language
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import syntax.AstSugar._
import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.{HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 2/18/19
  */
class FunctionReturnTypeRewritePropSpec extends PropSpec with Checkers {
  property("Finds in the graph") {
    val graph = VersionedHyperGraph(
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

  property("Finds 2 different usages of the same function in the graph") {
    val graph = VersionedHyperGraph(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(I("f")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(3), HyperTermId(4)), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(5), HyperTermId(6)), EmptyMetadata),
      HyperEdge(HyperTermId(7), HyperTermIdentifier(I("f")), Seq(HyperTermId(8), HyperTermId(9), HyperTermId(10)), EmptyMetadata),

      HyperEdge(HyperTermId(27), HyperTermIdentifier(I("f")), Seq(HyperTermId(28), HyperTermId(29), HyperTermId(210)), EmptyMetadata)
    )
    val expectedNewEdges = Set(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(7), HyperTermId(6)), EmptyMetadata),
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(27), HyperTermId(6)), EmptyMetadata)
    )

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    check(actualNewEdges == expectedNewEdges)
  }

  property("Finds 2 different functions in the graph") {
    val graph = VersionedHyperGraph(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(I("f")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(3), HyperTermId(4)), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(5), HyperTermId(6)), EmptyMetadata),
      HyperEdge(HyperTermId(7), HyperTermIdentifier(I("f")), Seq(HyperTermId(8), HyperTermId(9), HyperTermId(10)), EmptyMetadata),

      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(21), HyperTermId(22)), EmptyMetadata),
      HyperEdge(HyperTermId(21), HyperTermIdentifier(I("g")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(22), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(23), HyperTermId(24)), EmptyMetadata),
      HyperEdge(HyperTermId(24), HyperTermIdentifier(Language.mapTypeId), Seq(HyperTermId(25), HyperTermId(26)), EmptyMetadata),
      HyperEdge(HyperTermId(27), HyperTermIdentifier(I("g")), Seq(HyperTermId(8), HyperTermId(9), HyperTermId(10)), EmptyMetadata)
    )
    val expectedNewEdges = Set(
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(7), HyperTermId(6)), EmptyMetadata),
      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.typeId), Seq(HyperTermId(27), HyperTermId(26)), EmptyMetadata)
    )

    val newGraph = FunctionReturnTypeRewrite(new RewriteSearchState(graph)).graph
    val actualNewEdges = newGraph.edges -- graph.edges

    check(actualNewEdges == expectedNewEdges)
  }
}
