package synthesis.search.action.operators.thesy

import structures.Metadata
import structures.generic.HyperGraph
import synthesis.search.Operator
import synthesis.search.action.operators.LetAction
import synthesis.search.rewrite.operators.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.rewrite.{RewriteRulesDB, RewriteSearchState}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.AnnotatedTree._
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

case class SyGuERewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
  require(terms.forall(_.subtrees.isEmpty))
  require(terms.forall(_.root.annotation.nonEmpty))
  require(terms.forall(_.root.annotation.get.root == Language.mapTypeId))

  override lazy val rewriteRules: Set[Operator[RewriteSearchState]] = {
    terms.flatMap({ t =>
      val typ = t.root.annotation.get
      // sources are all typed expressions needed
      val params = typ.subtrees.dropRight(1).zipWithIndex.map(tup => identifierOnly(Identifier(s"?autovar${tup._2}", annotation = Some(tup._1))))
      val premisedParams = params.map(p =>
          withoutAnnotations(Language.andCondBuilderId, Seq(
            identifierOnly(Language.trueId),
            withoutAnnotations(SyGuERewriteRules.sygusCreatedId, Seq(p)
          ))
        ))
      assert(params.nonEmpty)

      val term = {
        val premise =
          if (params.size > 1) withoutAnnotations(Language.limitedAndCondBuilderId, premisedParams)
          else premisedParams.head
        val conclusion = withoutAnnotations(Language.limitedAndCondBuilderId, Seq(
          withoutAnnotations(Language.andCondBuilderId, Seq(
            identifierOnly(Language.trueId),
            withoutAnnotations(SyGuERewriteRules.sygusCreatedId, Seq(withoutAnnotations(t.root, params)))
          ))
        ))
        withoutAnnotations(Language.limitedDirectedLetId, Seq(premise, conclusion))
      }

      new LetAction(term, cleanTypes = false).rules
    })
  }

  override protected def ruleTemplates: Set[AnnotatedTree] = Set.empty

  override protected def metadata: Metadata = SyGuERewriteRules.SyGuSMetadata
}

object SyGuERewriteRules {
  val sygusCreatedId = Identifier("sygusCreated")

  private val createdSearchGraph = {
    val parser = new TranscalParser()
    Programs.destructPattern(parser.parseExpression(s"${SyGuERewriteRules.sygusCreatedId.literal} ?x"))
  }
  private val sygusCreatedSearchHole = createdSearchGraph.findByEdgeType(ExplicitTerm(HyperTermIdentifier(SyGuERewriteRules.sygusCreatedId)))
    .head.sources.head.asInstanceOf[ReferenceTerm[HyperTermId]]

  def getSygusCreatedNodes(graph: HyperGraph[HyperTermId, HyperTermIdentifier]): Set[HyperTermId] = {
    graph.findSubgraph[Int](createdSearchGraph).map(_._1(sygusCreatedSearchHole.id))
  }

  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }

}