package synthesis.actions.operators.SPBE

import structures.HyperGraphManyWithOrderToOneLike.HyperEdgePattern
import structures.immutable.VersionedHyperGraph
import structures.{EmptyMetadata, Hole, HyperEdge, Metadata}
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.search.Operator
import synthesis.{HyperTermId, HyperTermIdentifier, Programs, RewriteRulesDB}
import transcallang.{AnnotatedTree, Identifier, Language}

case class SyGuSRewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
  assert(terms.forall(_.subtrees.isEmpty))
  assert(terms.forall(_.root.annotation.nonEmpty))
  assert(terms.forall(_.root.annotation.get.root == Language.mapTypeId))

  //  def getExpressionAsTypedTree(typeTerm: AnnotatedTree): AnnotatedTree =
  //    AnnotatedTree.identifierOnly(expressionId.copy(literal = expressionId.literal, annotation = Some(typeTerm)))
  private def getRewrites: Set[Operator[RewriteSearchState]] = {
    terms.map({ t =>
      val typ = t.root.annotation.get
      val params = typ.subtrees.dropRight(1).zipWithIndex.map(tup => AnnotatedTree.identifierOnly(Identifier(s"?autovar${tup._2}", annotation = Some(tup._1))))
      // sources are all typed expressions needed
      val paramPatterns = Programs.destructPatterns(params)

      val premise = VersionedHyperGraph(paramPatterns.flatMap(_.edges): _*)

      val conclusion = VersionedHyperGraph(Programs.destructPattern(AnnotatedTree.withoutAnnotations(
        t.root.copy(annotation = Some(typ.subtrees.last)),
        params)).edges.toSeq: _*)

      val metaCreator: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata = {
        t: (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => metadata
      }

      new RewriteRule(premise, conclusion, metaCreator)
    })
  }

  override val rewriteRules: Set[Operator[RewriteSearchState]] = getRewrites

  override protected def ruleTemplates: Set[AnnotatedTree] = Set.empty

  override protected def metadata: Metadata = SyGuSRewriteRules.SyGuSMetadata
}

object SyGuSRewriteRules {

  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }

}