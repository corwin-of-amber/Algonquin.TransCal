package synthesis.search.rewrites

import org.scalacheck.Gen
import org.scalacheck.Gen._
import structures.mutable._
import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.rewrites.PatternRewriteRule.HyperPattern
import synthesis.search.rewrites.Template.{ExplicitTerm, TemplateTerm}
import synthesis.{HyperTermId, HyperTermIdentifier}

package object operators {
  def CompactHyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[CompactHyperGraph[Node, Edge]] =
    edgeSource.map(CompactHyperGraph(_))

  val hyperTermIdentifierGen: Gen[HyperTermIdentifier] = synthesis.identifierGen.map(HyperTermIdentifier)
  val hyperTermIdGen: Gen[HyperTermId] = oneOf(1, 50).map(HyperTermId)

  val hyperGraphEdgeGen: Gen[HyperEdge[HyperTermId, HyperTermIdentifier]] = HyperEdgeGenFactory(hyperTermIdGen, hyperTermIdentifierGen)
  val hyperGraphGen: Gen[RewriteRule.HyperGraph] = CompactHyperGraphGenFactory(hyperGraphEdgeGen)

  val hyperPatternGen: Gen[HyperPattern] = hyperGraphGen
    .map(graph => graph.map(edge => HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](ExplicitTerm(edge.target), ExplicitTerm(edge.edgeType), edge.sources.map(ExplicitTerm[HyperTermId]), EmptyMetadata)))
    .map(g => structures.generic.HyperGraph(g.toSeq:_*))

  val rewriteRuleGen: Gen[PatternRewriteRule] = for {
    conditions <- hyperPatternGen
    destination <- hyperPatternGen
  } yield new PatternRewriteRule(conditions, destination, (_, _) => EmptyMetadata)
}
