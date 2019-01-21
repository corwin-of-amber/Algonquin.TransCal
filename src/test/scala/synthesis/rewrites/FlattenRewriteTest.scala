package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.{EmptyMetadata, HyperEdge}
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import language.{Language, TranscalParser}
import language.Language._
import structures.immutable.VocabularyHyperGraph
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.Template.ReferenceTerm


class FlattenRewriteTest extends PropSpec with Checkers  {

  property("flatten works") {
    val edges = Set(HyperEdge(HyperTermId(0), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(1), HyperTermId(2)), EmptyMetadata),
      HyperEdge(HyperTermId(1), HyperTermIdentifier(Language.applyId), Seq(HyperTermId(3), HyperTermId(4), HyperTermId(5)), EmptyMetadata),
      HyperEdge(HyperTermId(2), HyperTermIdentifier(new Identifier("x")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(3), HyperTermIdentifier(new Identifier("f")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(4), HyperTermIdentifier(new Identifier("z")), Seq(), EmptyMetadata),
      HyperEdge(HyperTermId(5), HyperTermIdentifier(new Identifier("y")), Seq(), EmptyMetadata))
    val state = new RewriteSearchState(VocabularyHyperGraph(edges.toSeq: _*))
    val newEdge = FlattenRewrite(state).graph.edges -- state.graph.edges
    check(newEdge.size == 1)
    check(newEdge.head.sources.size == 2)
    check(newEdge.head.edgeType.identifier.literal == "@")
  }

}
