package synthesis.actions.operators.SPBE

import structures.HyperGraphManyWithOrderToOneLike.HyperEdgePattern
import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, Hole, HyperEdge, Metadata}
import synthesis.actions.operators.LetAction
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, RewriteRulesDB}
import transcallang.{AnnotatedTree, Identifier, Language}

case class SyGuSRewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
  assert(terms.forall(_.subtrees.isEmpty))
  assert(terms.forall(_.root.annotation.nonEmpty))
  assert(terms.forall(_.root.annotation.get.root == Language.mapTypeId))

  override lazy val rewriteRules: Set[Operator[RewriteSearchState]] = {
    terms.flatMap({ t =>
      val typ = t.root.annotation.get
      // sources are all typed expressions needed
      val params = typ.subtrees.dropRight(1).zipWithIndex.map(tup => AnnotatedTree.identifierOnly(Identifier(s"?autovar${tup._2}", annotation = Some(tup._1))))
      assert(params.nonEmpty)
      val term = {
        val premise = AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, params)
        val conclusion = AnnotatedTree.withoutAnnotations(t.root.copy(annotation = Some(typ.subtrees.last)), params)
        AnnotatedTree.withoutAnnotations(Language.directedLetId, Seq(premise, conclusion))
      }

      new LetAction(term).rules
    })
  }

  override protected def ruleTemplates: Set[AnnotatedTree] = Set.empty

  override protected def metadata: Metadata = SyGuSRewriteRules.SyGuSMetadata
}

object SyGuSRewriteRules {

  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }

}