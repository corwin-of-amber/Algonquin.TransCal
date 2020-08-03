package synthesis.search.actions

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Matchers}
import structures.{EmptyMetadata, HyperEdge}
import synthesis.search.actions.LocateAction.LocateMetadata
import synthesis.search.rewrites.RewriteRule
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}
import synthesis.search.{ActionSearchState, Operator}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language, TranscalParser}

class LocateActionTest extends FunSuite with Matchers with LazyLogging {

  test("can locate the whole term") {
    val rules: Set[RewriteRule] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat ?l = l match ((⟨⟩ => ⟨⟩) / (?xs :: ?xss => xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = new ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.letId)), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = structures.mutable.HyperGraph.empty.+(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(Identifier("anchor")), template)(state.deepCopy())
    val newEdges = newState.programs.queryGraph.edges.diff(state.programs.queryGraph.edges)
    newEdges.size should be (1)
    newEdges.head.edgeType.identifier.literal should be ("anchor")
    newEdges.head.metadata.count(_.isInstanceOf[LocateMetadata]) should be (1)
  }

  test("locate two out of two (rewrite rule should return all possibilities after one apply)") {
    val rules: Set[RewriteRule] = Set.empty
    logger.info("Using these rewrite rules:")
    logger.info(rules.mkString("\n"))
    val mainTerm = (new TranscalParser).apply("concat ?l = l match ((⟨⟩ => ⟨⟩) / (?xs :: ?xss => xs ++ concat xss))   [++]")
    val progs = Programs(mainTerm)
    val state = new ActionSearchState(progs, rules)
    val equalEdge = HyperEdge[TemplateTerm[HyperTermId], TemplateTerm[HyperTermIdentifier]](
      ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Identifier("⇒"))), Seq(ReferenceTerm(1), ReferenceTerm(2)), EmptyMetadata
    )
    val template = structures.mutable.HyperGraph.empty.+(equalEdge)
    val newState = new LocateAction(HyperTermIdentifier(Identifier("anchor")), template)(state.deepCopy())
    val newEdges = newState.programs.queryGraph.edges.diff(state.programs.queryGraph.edges)
    newEdges.count(_.edgeType.identifier.literal == "anchor") should be (1)
  }
}
