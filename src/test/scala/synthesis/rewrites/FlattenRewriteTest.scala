package synthesis.rewrites

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.{EmptyMetadata, HyperEdge}
import syntax.AstSugar.Term
import syntax.{Identifier, Tree}
import language.TranscalParser
import language.Language._
import synthesis.Programs
import synthesis.rewrites.Template.ReferenceTerm


class FlattenRewriteTest extends PropSpec with Checkers  {

  property("flatten works") {
    val term = new TranscalParser().apply("(a b) c d")
    val state = new RewriteSearchState(Programs.destruct(term))
    val newEdge = FlattenRewrite(state).graph.edges -- state.graph.edges
    check(newEdge.size == 1)
    check(newEdge.head.sources.size == 2)
    check(newEdge.head.edgeType.identifier.literal == "@")
  }

}
