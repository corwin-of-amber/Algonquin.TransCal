package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import structures.immutable.{HyperGraphManyWithOrderToOne, VocabularyHyperGraph}
import syntax.AstSugar.Term
import syntax.Identifier
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, SimpleRewriteRulesDB}
import language.TranscalParser
import synthesis.rewrites.RewriteRule
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

class LocateActionTest extends FunSuite with Matchers with LazyLogging {

  test("can locate the whole term") {
    val rules: Set[RewriteRule] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat = ((⟨⟩ ↦ ⟨⟩) / (?xs :: ?xss ↦ xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = new ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(new Identifier("="))), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = VocabularyHyperGraph.empty.addEdge(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(new Identifier("anchor", kind = Programs.Kinds.NonConstructable.toString)), template)(state)
    val newEdges = newState.programs.hyperGraph.edges.diff(state.programs.hyperGraph.edges)
    newEdges.size should be (1)
    newEdges.head.edgeType.identifier.literal should be ("anchor")
    newEdges.head.metadata.count(_.isInstanceOf[LocateMetadata]) should be (1)
  }

  test("locate one out of two") {
    val rules: Set[RewriteRule] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat = ((⟨⟩ ↦ ⟨⟩) / (?xs :: ?xss ↦ xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = new ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(new Identifier("↦"))), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = VocabularyHyperGraph.empty.addEdge(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(new Identifier("anchor", kind = Programs.Kinds.NonConstructable.toString)), template)(state)
    val newEdges = newState.programs.hyperGraph.edges.diff(state.programs.hyperGraph.edges)
    newEdges.size should be (1)
    newEdges.head.edgeType.identifier.literal should be ("anchor")
    newEdges.head.metadata.count(_.isInstanceOf[LocateMetadata]) should be (1)
  }
}
