package synthesis

import relentless.BasicSignature
import structures.immutable.HyperGraphManyWithOrderToOne
import structures.{EmptyMetadata, HyperEdge}
import syntax.AstSugar._
import syntax.{AstSugar, Identifier}
import synthesis.rewrites.RewriteRule
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

/**
  * @author tomer
  * @since 12/27/18
  */
trait RewriteRulesDB {
  protected def vars: Set[Identifier]
  protected def ruleTemplates: Seq[Term]
  lazy val rewriteRules: Seq[RewriteRule] = ruleTemplates.flatMap(ruleTemplatesToRewriteRules)

  private def ruleTemplatesToRewriteRules(ruleTemplate: Term): Set[RewriteRule] = {
    def termToHyperPattern(term: Term): HyperPattern = {
      val ruleTemplateGraph = Programs(term).hyperGraph
      val hyperPatternEdges = ruleTemplateGraph.edges.filterNot(edge=>vars.contains(edge.edgeType.identifier)).map(edge => {
        HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
          ReferenceTerm(edge.target.id),
          ExplicitTerm(edge.edgeType),
          edge.sources.map(hti => ReferenceTerm[HyperTermId](hti.id)),
          EmptyMetadata
        )
      })
      HyperGraphManyWithOrderToOne(hyperPatternEdges)
    }
    ruleTemplate.root match {
      case AstSugar.`=` =>
        val leftTerm = ruleTemplate.subtrees.head
        val rightTerm = ruleTemplate.subtrees.last
        println(s"equal $leftTerm $rightTerm")
        val leftPattern = termToHyperPattern(leftTerm)
        val rightPattern = termToHyperPattern(rightTerm)
        Set(
          new RewriteRule(leftPattern, rightPattern, (a, b) => EmptyMetadata),
          new RewriteRule(rightPattern, leftPattern, (a, b) => EmptyMetadata)
        )
      case relentless.rewriting.RewriteRule.`=>` =>
        val leftTerm = ruleTemplate.subtrees.head
        val rightTerm = ruleTemplate.subtrees.last
        println(s"directional rewrite $leftTerm $rightTerm")
        val conditions = termToHyperPattern(leftTerm)
        val destination = termToHyperPattern(rightTerm)
        Set(new RewriteRule(conditions, destination, (a, b) => EmptyMetadata))
      case _ =>
        println("unknown")
        Set()
    }
  }
}

object SimpleRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set()

  override protected val ruleTemplates: Seq[Term] = Seq(
    ~ BasicSignature.tt =:= BasicSignature.ff,
    ~ BasicSignature.ff =:= BasicSignature.tt
  )
}

object Main extends App {
  println(SimpleRewriteRulesDB.rewriteRules)
  println(SimpleRewriteRulesDB.rewriteRules.size)
}
