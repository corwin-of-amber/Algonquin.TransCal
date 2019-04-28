package synthesis.actions.operators.SPBE

import structures.Metadata
import syntax.{Tree}
import synthesis.RewriteRulesDB
import transcallang.{Identifier, Language, TranscalParser}

class SyGuSRewriteRules(terms: Set[Tree[Identifier]]) extends RewriteRulesDB {
  assert(terms.forall(_.root == Language.typeBuilderId))
  assert(terms.forall(_.subtrees(0).subtrees.isEmpty))

  private val parser = new TranscalParser
  private val expressionTerm = new Tree(Identifier("Expression"))

  private def getExpressionTyped(typeTerm: Tree[Identifier]): Tree[Identifier] = {
    new Tree(Language.typeBuilderId, List(expressionTerm, typeTerm))
  }

  private def getRecursiveExpression(term: Tree[Identifier]): Tree[Identifier] = {
    val name = term.subtrees(0).root
    val typ = term.subtrees(1)
    typ.root match {
      case Language.mapTypeId => typ.subtrees.head match {
        case Language.tupleId => new Tree(name, typ.subtrees.head.subtrees.map(getExpressionTyped))
        case _ => new Tree(name, List(getExpressionTyped(typ.subtrees.head)))
      }
      case _ => term.subtrees.head
    }
  }

  override protected def ruleTemplates: Set[Tree[Identifier]] = terms.map({ t =>
    t.subtrees(1).root match {
      case Language.mapTypeId => new Tree(Language.directedLetId, List(getExpressionTyped(t.subtrees(1).subtrees(1)), getRecursiveExpression(t)))
      case _ => new Tree(Language.directedLetId, List(getExpressionTyped(t.subtrees(1)), getRecursiveExpression(t)))
    }
  })

  override protected def metadata: Metadata = SyGuSRewriteRules.SyGuSMetadata
}

object SyGuSRewriteRules {
  object SyGuSMetadata extends Metadata {
    override protected def toStr: String = "SyGuSMetadata"
  }
}