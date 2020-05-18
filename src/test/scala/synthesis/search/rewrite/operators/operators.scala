package synthesis.search.rewrite.operators

import org.scalacheck.Gen
import org.scalacheck.Gen._
import structures.mutable._
import structures.{EmptyMetadata, HyperEdge}
import synthesis.{HyperTermId, HyperTermIdentifier}
import synthesis.search.rewrite.RewriteSearchState
import synthesis.search.rewrite.operators.RewriteRule.HyperPattern
import synthesis.search.rewrite.RewriteSearchState.HyperGraph
import synthesis.search.rewrite.operators.Template.{ExplicitTerm, TemplateTerm}

package object operators {
  def CompactHyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[CompactHyperGraph[Node, Edge]] =
    edgeSource.map(CompactHyperGraph(_))

  val hyperTermIdentifierGen: Gen[HyperTermIdentifier] = synthesis.identifierGen.map(HyperTermIdentifier)
  val hyperTermIdGen: Gen[HyperTermId] = oneOf(1, 50).map(HyperTermId)

  val hyperGraphEdgeGen: Gen[HyperEdge[HyperTermId, HyperTermIdentifier]] = HyperEdgeGenFactory(hyperTermIdGen, hyperTermIdentifierGen)
  val hyperGraphGen: Gen[HyperGraph] = CompactHyperGraphGenFactory(hyperGraphEdgeGen)

  val hyperPatternGen: Gen[HyperPattern] = hyperGraphGen
    .map(graph => graph.map(edge => HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](ExplicitTerm(edge.target), ExplicitTerm(edge.edgeType), edge.sources.map(ExplicitTerm[HyperTermId]), EmptyMetadata)))
    .map(g => structures.immutable.HyperGraph(g.toSeq:_*))

  val rewriteRuleGen: Gen[RewriteRule] = for {
    conditions <- hyperPatternGen
    destination <- hyperPatternGen
  } yield new RewriteRule(conditions, destination, (_, _) => EmptyMetadata)

  val rewriteSearchStateGen: Gen[RewriteSearchState] = hyperGraphGen.map(new RewriteSearchState(_))
}
