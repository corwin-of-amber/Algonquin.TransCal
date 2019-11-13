package synthesis.rewrites

import org.scalatest.{Matchers, PropSpec}
import structures.immutable.CompactHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language}


class FlattenRewriteTest extends PropSpec with Matchers  {

  property("flatten works") {
    val edges = Set(HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(Identifier("x")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(3), HyperTermIdentifier(Identifier("f")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(Identifier("z")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(5), HyperTermIdentifier(Identifier("y")), Seq(), EmptyMetadata))
    val progs = Programs(CompactHyperGraph(edges.toSeq: _*))
    val state = new RewriteSearchState(CompactHyperGraph(progs.hyperGraph.toSeq: _*))
    val newEdges = FlattenRewrite(state).graph.edges -- progs.hyperGraph.edges
    val newEdge = newEdges.filter(_.sources.size == 4)
    newEdges should have size 2
    newEdge.head.sources should have size 4
    newEdge.head.edgeType.identifier.literal shouldEqual "@"
    newEdge.head.sources.head.id shouldEqual 3
    newEdge.head.sources(1).id shouldEqual 4
    newEdge.head.sources.last.id shouldEqual 2
  }

//  property("flatten works on multiple depths") {
//    val edges = Set(HyperEdge(HyperTermId(10), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(0), HyperTermId(2)), EmptyMetadata),
//      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
//      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
//      HyperEdge(HyperTermId(2), HyperTermIdentifier(Identifier("x")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(3), HyperTermIdentifier(Identifier("f")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(4), HyperTermIdentifier(Identifier("z")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(5), HyperTermIdentifier(Identifier("y")), Seq(), EmptyMetadata))
//    val state = new RewriteSearchState(VocabularyHyperGraph(edges.toSeq: _*))
//    val newEdges = FlattenRewrite(state).graph.edges -- state.graph.edges
//    newEdges.exists(e => e.sources.size shouldEqual 5)
//  }

  property("flatten works on funcs") {
    val edges = Set(
      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
      HyperEdge(HyperTermId(3), HyperTermIdentifier(Identifier("f")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(Identifier("z")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(5), HyperTermIdentifier(Identifier("y")), Seq(), EmptyMetadata))
    val progs = Programs(CompactHyperGraph(edges.toSeq: _*))
    val state = new RewriteSearchState(CompactHyperGraph(progs.hyperGraph.toSeq: _*))
    val flattened = FlattenRewrite(state)
    val newEdge = (flattened.graph.edges -- progs.hyperGraph.edges).filter(e => e.edgeType.identifier.literal == "f")
    newEdge should have size 1
    newEdge.head.sources should have size 2
    newEdge.head.sources.head.id shouldEqual 4
    newEdge.head.sources.last.id shouldEqual 5
  }
}
