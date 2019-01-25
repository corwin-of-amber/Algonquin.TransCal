package synthesis.actions.operators

import semantics.LambdaCalculus
import syntax.{Identifier, Scheme, Tree}
import synthesis.actions.ActionSearchState
import LambdaCalculus.↦⁺
import language.Language
import structures._
import syntax.AstSugar._
import synthesis.actions.operators.LetAction.LetMetadata
import synthesis.rewrites.{RewriteRule, Template}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import synthesis.rewrites.RewriteRule.HyperPattern
import synthesis.rewrites.RewriteSearchState.HyperGraph
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm, TemplateTerm}

/** Let action adds a rewrite rule to show the equality between the two templates.
  * This action also adds all equalities
  * This action does not add the templates to the graph. To also add the templates to the graph see DefAction.
  *
  * @author tomer
  * @since 11/18/18
  */
class LetAction(term: Term) extends Action {
  // TODO: check what skolemize was
  // TODO: Take care of splitting by adding inner lets

  // Beta reduction is done by adding rewrite rules and using flatten

  assert(Language.builtinDefinitions contains term.root.literal.toString)

  // Start by naming lambdas and removing the bodies into rewrites.
  // I can give temporary name and later override them by using merge nodes
  def renameLambdas(t: Term): (Set[RewriteRule], Term) = {
    val results = t.subtrees map renameLambdas
    val subtrees = results.map(_._2)
    val rewrites = results.flatMap(_._1)
    t.root match {
      case Language.lambdaId =>
        val knownHoles: Set[Set[Term]] = {
          val definitions = t.nodes.filter(n => n.root.literal.toString.startsWith("?"))
          val partners = definitions.map(i => new Tree(new Identifier(i.root.literal.toString.drop(1), i.root.kind, i.root.ns)))
          definitions zip partners map (t => Set(t._1, t._2)) toSet
        }

        val newFunc = LetAction.functionNamer()
        val params = if(subtrees(0).root == Language.tupleId) subtrees(0).subtrees else List(subtrees(0))
        val condTerm = new Tree(newFunc, params)
        val destTerm = subtrees(1)
        val pattern = Programs.destructPattern(condTerm, knownHoles)
        val conditions: HyperPattern = {
          val rootEdge = conditions.findEdges(ExplicitTerm(HyperTermIdentifier(newFunc))).head
          val newRootEdge = rootEdge.copy(sources = rootEdge.sources :+ RepetitionTerm.rep0[HyperTermId, Int](Int.MaxValue, Ignored[HyperTermId, Int]()).get)
          conditions.addEdge(newRootEdge).removeEdge(rootEdge)
        }
        val destination = Programs.destructPattern(destTerm, knownHoles)
        ((RewriteRule(conditions, destination, metadataCreator(newFunc)) :: rewrites).toSet, new Tree(newFunc))
      case _ => (rewrites.toSet, new Tree(t.root, subtrees))
    }
  }

  protected val (rewrites, updatedTerm) = renameLambdas(term)
  protected val mainRewrite = {
    // TODO: fix copy paste
    val knownHoles: Set[Set[Term]] = {
      val definitions = updatedTerm.nodes.filter(n => n.root.literal.toString.startsWith("?"))
      val partners = definitions.map(i => new Tree(new Identifier(i.root.literal.toString.drop(1), i.root.kind, i.root.ns)))
      definitions zip partners map (t => Set(t._1, t._2)) toSet
    }

    RewriteRule(Programs.destructPattern(updatedTerm.subtrees(0), knownHoles),
      Programs.destructPattern(updatedTerm.subtrees(1), knownHoles),
      metadataCreator(updatedTerm.subtrees(0).root))
  }

  def metadataCreator(funcName: Identifier): (Map[Int, HyperTermId], Map[Int, HyperTermIdentifier]) => Metadata = {
    (m1: Map[Int, HyperTermId], m2: Map[Int, HyperTermIdentifier]) => LetMetadata(funcName)
  }

  override def apply(state: ActionSearchState): ActionSearchState = {
    // Take main expression and create a rewrite
    new ActionSearchState(state.programs, state.rewriteRules ++ rewrites)
  }
}

object LetAction {
  protected val functionNamer: () => Identifier = {
    val creator = Stream.from(language.Language.arity.size).iterator
    () => I(s"f${creator.next()}")
  }

  case class LetMetadata(funcName: Identifier) extends Metadata {
    override val toStr = s"LetMetadata($funcName)"
  }
}