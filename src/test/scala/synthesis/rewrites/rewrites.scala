package synthesis

import org.scalacheck.Gen
import org.scalacheck.Gen._
import structures.immutable._
import structures.{EmptyMetadata, HyperEdge}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}

package object rewrites {
  def CompactHyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[CompactHyperGraph[Node, Edge]] =
    edgeSource.map(CompactHyperGraph(_))

  val hyperTermIdentifierGen: Gen[HyperTermIdentifier] = identifierGen.map(HyperTermIdentifier)
  val hyperTermIdGen: Gen[HyperTermId] = oneOf(1, 50).map(HyperTermId)

  val hyperGraphEdgeGen: Gen[HyperEdge[HyperTermId, HyperTermIdentifier]] = HyperEdgeGenFactory(hyperTermIdGen, hyperTermIdentifierGen)
  val hyperGraphGen: Gen[HyperGraph] = CompactHyperGraphGenFactory(hyperGraphEdgeGen)

  val hyperPatternGen: Gen[HyperPattern] = hyperGraphGen
    .map(graph => graph.map(edge => HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](ExplicitTerm(edge.target), ExplicitTerm(edge.edgeType), edge.sources.map(ExplicitTerm[HyperTermId]), EmptyMetadata)))
    .map(g => HyperGraphManyWithOrderToOne(g.toSeq:_*))

  val rewriteRuleGen: Gen[RewriteRule] = for {
    conditions <- hyperPatternGen
    destination <- hyperPatternGen
  } yield new RewriteRule(conditions, destination, (a, b) => EmptyMetadata)

  val rewriteSearchStateGen: Gen[RewriteSearchState] = hyperGraphGen.map(new RewriteSearchState(_))
}
