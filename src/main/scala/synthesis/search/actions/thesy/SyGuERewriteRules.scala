package synthesis.search.actions.thesy

import structures.{HyperGraph, Metadata}
import synthesis.search.actions.LetAction
import synthesis.search.actions.thesy.SyGuERewriteRules.{SyGuEMetadata, SyGuEMetadataBranch}
import synthesis.search.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.rewrites.{RewriteRule, RewriteRulesDB}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.AnnotatedTree._
import transcallang.{AnnotatedTree, Identifier, Language, TranscalParser}

case class SyGuERewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
  require(terms.forall(_.subtrees.isEmpty))
  require(terms.forall(_.root.annotation.nonEmpty))
  require(terms.forall(_.root.annotation.get.root == Language.mapTypeId))

  def metadataCreator(id: Int): structures.Match[HyperTermId, HyperTermIdentifier, Int] => Metadata = m => {
    SyGuEMetadataBranch(id, m.edges.flatMap(e => e.metadata.collect({
      case m: SyGuEMetadata => Some(m)
      case _ => None
    }).flatten))
  }

  override lazy val rewriteRules: Set[RewriteRule] = {
    terms.flatMap({ t =>
      val typ = t.root.annotation.get
      // sources are all typed expressions needed
      val params = typ.subtrees.dropRight(1).zipWithIndex.map(tup => identifierOnly(Identifier(s"?autovar${tup._2}", annotation = Some(tup._1))))
      val premisedParams = params.map(p =>
          withoutAnnotations(Language.andCondBuilderId, Seq(
            identifierOnly(Language.trueId),
            withoutAnnotations(SyGuERewriteRules.sygueCreatedId, Seq(p)
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
            withoutAnnotations(SyGuERewriteRules.sygueCreatedId, Seq(withoutAnnotations(t.root, params)))
          ))
        ))
        withoutAnnotations(Language.limitedDirectedLetId, Seq(premise, conclusion))
      }

      new LetAction(term, cleanTypes = false).rules.zipWithIndex
        .map({case (r, i) => r.withTermString(s"SyGuS[${t.root.literal}]")})
//        .map({case (r, i) => r.withTermString(s"SyGuS[${t.root.literal}]").registerMetadataCreator(metadataCreator(i))})
    })
  }

  override protected def ruleTemplates: Set[AnnotatedTree] = Set.empty

  override protected def metadata: Metadata = SyGuERewriteRules.SyGuSMetadata
}

object SyGuERewriteRules {
  val sygueCreatedId = Identifier("sygusCreated")

  trait SyGuEMetadata extends Metadata {
    override protected def toStr: String = "SyGuEMetadata"
  }

  case class SyGuEMetadataBranch(id: Int, params: Set[SyGuEMetadata]) extends SyGuEMetadata
  case class SyGuEMetadataLeaf(id: HyperTermIdentifier) extends SyGuEMetadata

  private val createdSearchGraph = {
    val parser = new TranscalParser()
    Programs.destructPattern(parser.parseExpression(s"${SyGuERewriteRules.sygueCreatedId.literal} ?x"))
  }
  private val sygusCreatedSearchHole = createdSearchGraph.findByEdgeType(ExplicitTerm(HyperTermIdentifier(SyGuERewriteRules.sygueCreatedId)))
    .head.sources.head.asInstanceOf[ReferenceTerm[HyperTermId]]

  def getSygusCreatedNodes(graph: HyperGraph[HyperTermId, HyperTermIdentifier]): Set[HyperTermId] = {
    graph.findSubgraph[Int](createdSearchGraph).map(_.nodeMap(sygusCreatedSearchHole.id))
  }

  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }

}