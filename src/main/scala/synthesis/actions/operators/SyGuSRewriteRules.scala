package synthesis.actions.operators

import structures.generic.HyperGraph
import structures.{HyperGraphLike, Metadata}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, RewriteRulesDB}
import synthesis.rewrites.RewriteSearchState
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.Operator
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}
import transcallang.AnnotatedTree._

case class SyGuSRewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
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
            withoutAnnotations(SyGuSRewriteRules.sygusCreatedId, Seq(p)
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
            withoutAnnotations(SyGuSRewriteRules.sygusCreatedId, Seq(withoutAnnotations(t.root, params)))
          ))
        ))
        withoutAnnotations(Language.limitedDirectedLetId, Seq(premise, conclusion))
      }

      new LetAction(term, cleanTypes = false).rules
    })
  }

  override protected def ruleTemplates: Set[AnnotatedTree] = Set.empty

  override protected def metadata: Metadata = SyGuSRewriteRules.SyGuSMetadata
}

object SyGuSRewriteRules {
  val sygusCreatedId = Identifier("sygusCreated")

  private val createdSearchGraph = {
    val parser = new TranscalParser()
    Programs.destructPattern(parser.parseExpression(s"${SyGuSRewriteRules.sygusCreatedId.literal} ?x"))
  }
  private val sygusCreatedSearchHole = createdSearchGraph.findByEdgeType(ExplicitTerm(HyperTermIdentifier(SyGuSRewriteRules.sygusCreatedId)))
    .head.sources.head.asInstanceOf[ReferenceTerm[HyperTermId]]

  def getSygusCreatedNodes(graph: HyperGraph[HyperTermId, HyperTermIdentifier]): Set[HyperTermId] = {
    graph.findSubgraph[Int](createdSearchGraph).map(_._1(sygusCreatedSearchHole.id))
  }

  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }

}