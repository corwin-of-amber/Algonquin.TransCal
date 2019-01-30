package synthesis.rewrites

import language.Language
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.immutable.VocabularyHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import syntax.Identifier
import synthesis.{HyperTermId, HyperTermIdentifier}


class FlattenRewriteTest extends PropSpec with Checkers  {

  property("flatten works") {
    val edges = Set(HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(new Identifier("x")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(3), HyperTermIdentifier(new Identifier("f")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(new Identifier("z")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(5), HyperTermIdentifier(new Identifier("y")), Seq(), EmptyMetadata))
    val state = new RewriteSearchState(VocabularyHyperGraph(edges.toSeq: _*))
    val newEdges = FlattenRewrite(state).graph.edges -- state.graph.edges
    val newEdge = newEdges.filter(_.sources.size == 4)
    check(newEdges.size == 2)
    check(newEdge.head.sources.size == 4)
    check(newEdge.head.edgeType.identifier.literal == "@")
    check(newEdge.head.sources.head.id == 3)
    check(newEdge.head.sources(1).id == 4)
    check(newEdge.head.sources.last.id == 2)
  }

//  property("flatten works on multiple depths") {
//    val edges = Set(HyperEdge(HyperTermId(10), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(0), HyperTermId(2)), EmptyMetadata),
//      HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
//      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
//      HyperEdge(HyperTermId(2), HyperTermIdentifier(new Identifier("x")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(3), HyperTermIdentifier(new Identifier("f")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(4), HyperTermIdentifier(new Identifier("z")), Seq(), EmptyMetadata),
//      HyperEdge(HyperTermId(5), HyperTermIdentifier(new Identifier("y")), Seq(), EmptyMetadata))
//    val state = new RewriteSearchState(VocabularyHyperGraph(edges.toSeq: _*))
//    val newEdges = FlattenRewrite(state).graph.edges -- state.graph.edges
//    check(newEdges.exists(e => e.sources.size == 5))
//  }

  property("flatten works on funcs") {
    val edges = Set(
      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
      HyperEdge(HyperTermId(3), HyperTermIdentifier(new Identifier("f")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(new Identifier("z")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(5), HyperTermIdentifier(new Identifier("y")), Seq(), EmptyMetadata))
    val state = new RewriteSearchState(VocabularyHyperGraph(edges.toSeq: _*))
    val flattened = FlattenRewrite(state)
    val newEdge = (flattened.graph.edges -- state.graph.edges).filter(e => e.edgeType.identifier.literal == "f")
    check(newEdge.size == 1)
    check(newEdge.head.sources.size == 2)
    check(newEdge.head.sources.head.id == 4)
    check(newEdge.head.sources.last.id == 5)
  }
}
