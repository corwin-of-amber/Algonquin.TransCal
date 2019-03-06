package synthesis.actions.operators

import com.typesafe.scalalogging.LazyLogging
import transcallang.{Language, TranscalParser}
import org.scalatest.{FunSuite, Matchers}
import structures.immutable.VocabularyHyperGraph
import structures.{EmptyMetadata, HyperEdge}
import syntax.Identifier
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.LocateAction.LocateMetadata
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

class LocateActionTest extends FunSuite with Matchers with LazyLogging {

  test("can locate the whole term") {
    val rules: Set[Operator[RewriteSearchState]] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat ?l = l match ((⟨⟩ => ⟨⟩) / (?xs :: ?xss => xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.letId)), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = VocabularyHyperGraph.empty.addEdge(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(new Identifier("anchor")), template)(state)
    val newEdges = newState.programs.hyperGraph.edges.diff(state.programs.hyperGraph.edges)
    newEdges.size should be (1)
    newEdges.head.edgeType.identifier.literal should be ("anchor")
    newEdges.head.metadata.count(_.isInstanceOf[LocateMetadata]) should be (1)
  }

  test("locate two out of two (rewrite rule should return all possibilities after one apply)") {
    val rules: Set[Operator[RewriteSearchState]] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat ?l = l match ((⟨⟩ => ⟨⟩) / (?xs :: ?xss => xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(new Identifier("⇒"))), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = VocabularyHyperGraph.empty.addEdge(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(new Identifier("anchor")), template)(state)
    val newEdges = newState.programs.hyperGraph.edges.diff(state.programs.hyperGraph.edges)
    newEdges.count(_.edgeType.identifier.literal == "anchor") should be (1)
  }
}
