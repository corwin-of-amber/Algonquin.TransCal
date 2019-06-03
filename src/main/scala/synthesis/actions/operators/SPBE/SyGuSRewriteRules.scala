package synthesis.actions.operators.SPBE

import structures.Metadata
import synthesis.RewriteRulesDB
import transcallang.{AnnotatedTree, Identifier, Language}

case class SyGuSRewriteRules(terms: Set[AnnotatedTree]) extends RewriteRulesDB {
  assert(terms.forall(_.root.annotation.nonEmpty))

  private val expressionId = Identifier("Expression")

  def getExpressionAsTypedTree(typeTerm: AnnotatedTree): AnnotatedTree =
    AnnotatedTree.identifierOnly(expressionId.copy(literal=expressionId.literal + typeTerm.toString, annotation=Some(typeTerm)))

  private def getRecursiveExpression(term: AnnotatedTree): AnnotatedTree = {
    val name = term.root
    val typ = name.annotation.get
    typ.root match {
      case Language.mapTypeId => typ.subtrees.head match {
        case Language.tupleId => AnnotatedTree.withoutAnnotations(name, typ.subtrees.head.subtrees.map(getExpressionAsTypedTree))
        case _ => AnnotatedTree.withoutAnnotations(name, List(getExpressionAsTypedTree(typ.subtrees.head)))
      }
      case _ => term
    }
  }

  override protected def ruleTemplates: Set[AnnotatedTree] = terms.map({ t =>
    val typ = t.root.annotation.get
    typ.root match {
      case Language.mapTypeId => AnnotatedTree.withoutAnnotations(Language.directedLetId, List(getExpressionAsTypedTree(typ.subtrees(1)), getRecursiveExpression(t)))
      case _ => AnnotatedTree.withoutAnnotations(Language.directedLetId, List(getExpressionAsTypedTree(typ), getRecursiveExpression(t)))
    }
  })

  override protected def metadata: Metadata = SyGuSRewriteRules.SyGuSMetadata
}

object SyGuSRewriteRules {
  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }
}