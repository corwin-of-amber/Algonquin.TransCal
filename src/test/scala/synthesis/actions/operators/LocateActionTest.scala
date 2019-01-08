package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import structures.immutable.{HyperGraphManyWithOrderToOne, VocabularyHyperGraph}
import syntax.AstSugar.Term
import syntax.Identifier
import synthesis.actions.ActionSearchState
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, SimpleRewriteRulesDB}
import synthesis.language.OldParser
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

class LocateActionTest extends FunSuite with Matchers with LazyLogging {

  test("can locate the whole term") {
    val rules = SimpleRewriteRulesDB().rewriteRules
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new OldParser).apply("concat = ((⟨⟩ ↦ ⟨⟩) / (?xs :: ?xss ↦ xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = new ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(new Identifier("="))), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = VocabularyHyperGraph.empty.addEdge(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(new Identifier("anchor", kind = Programs.Kinds.NonConstructable.toString)), template)(state)
    val newRules = newState.rewriteRules.diff(rules)
    newRules.size should be (1)
  }

}
