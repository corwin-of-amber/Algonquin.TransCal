package synthesis

import org.scalacheck.Gen
import org.scalacheck.Gen._
import structures.{EmptyMetadata, HyperEdge}
import structures.immutable.{HyperEdgeGenFactory, HyperGraphGenFactory, HyperGraphManyWithOrderToOne}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, TemplateTerm}

package object rewrites {
  val hyperTermIdentifierGen: Gen[HyperTermIdentifier] = identifierGen.map(HyperTermIdentifier)
  val hyperTermIdGen: Gen[HyperTermId] = oneOf(1, 50).map(HyperTermId)

  val hyperGraphEdgeGen: Gen[HyperEdge[HyperTermId, HyperTermIdentifier]] = HyperEdgeGenFactory(hyperTermIdGen, hyperTermIdentifierGen)
  val hyperGraphGen: Gen[HyperGraph] = HyperGraphGenFactory(hyperGraphEdgeGen)

  val hyperPatternGen: Gen[HyperPattern] = hyperGraphGen
    .map(graph => graph.edges.map(edge => HyperEdge[TemplateTerm, TemplateTerm](ExplicitTerm(edge.target), ExplicitTerm(edge.edgeType), edge.sources.map(ExplicitTerm), EmptyMetadata)))
    .map(HyperGraphManyWithOrderToOne(_))

  val rewriteRuleGen: Gen[RewriteRule] = for {
    conditions <- hyperPatternGen
    destination <- hyperPatternGen
  } yield new RewriteRule(conditions, destination, (a, b) => EmptyMetadata)

  val rewriteSearchStateGen: Gen[RewriteSearchState] = hyperGraphGen.map(new RewriteSearchState(_))
}
