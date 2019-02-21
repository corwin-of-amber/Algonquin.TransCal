package synthesis

import com.typesafe.scalalogging.LazyLogging
import language.TranscalParser
import relentless.BasicSignature._
import relentless.rewriting.RewriteRule._
import structures.{EmptyMetadata, Metadata}
import syntax.AstSugar._
import syntax.Identifier
import synthesis.actions.operators.LetAction
import synthesis.rewrites.{FlattenRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator

/**
  * @author tomer
  * @since 12/27/18
  */
trait RewriteRulesDB extends LazyLogging {
  protected def vars: Set[Identifier]

  protected def ruleTemplates: Set[Term]

  protected def metadata: Metadata

  lazy val rewriteRules: Set[Operator[RewriteSearchState]] = Set[Operator[RewriteSearchState]](FlattenRewrite) ++ ruleTemplates.flatMap(ruleTemplatesToRewriteRules)

  private def ruleTemplatesToRewriteRules(ruleTemplate: Term): Set[RewriteRule] = new LetAction(ruleTemplate).rules
}

class SimpleRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z, `x'`, xs).map(_.root)

  private val parser = new TranscalParser

  private val templates: Set[String] = Set(
    "(true ⇒ ?y) >> id y",
    "(false ⇒ ?y) >> false",
    "~true = false",
    "~false = true",
    "?x / false >> id x",
    "false / ?x >> id x",
    "id (id ?x) >> id x",

    "?x == ?x' = x' ∈ { x }",
    "elem(?x, (?x' :: ?xs')) = ((x == x') \\/ elem(x, xs'))",
    "~(?x == ?y) = (x != y)",
    "~(?x ∈ ?y) = (x ∉ y)",
    "(?x ∉ ?xs) = { x } ‖ xs",
    "~(?x \\/ ?y) = (~x /\\ ~y)",
    "~(?x /\\ ?y) = (~x \\/ ~y)",
    "((?x ‖ ?xs) /\\ (?y ‖ xs)) = ((x ∪ y) ‖ xs)",
    "((?xs ‖ ?x) /\\ (xs ‖ ?y)) = (xs ‖ (x ∪ y))",
    "elem(?x, ?xs) = x ∈ elems(xs)",
    "elems(?x' :: ?xs') = ({x'} ∪ elems(xs'))", // <-- this one is somewhat superfluous?

    "(?y :+ ?x) = (y ++ (x :: ⟨⟩))",
    "⟨⟩ ++ ?xs' >> id xs'",
    "?xs' ++ ⟨⟩ >> id xs'",

    "((?x < ?y) ||| true) >> (x ≤ y)",
    "(?x ≤ ?y) ||> min(x, y) >> id x",
    "(?x ≤ ?y) ||> min(y, x) >> id x",
    //    min(x, y) =:> min(y,x),

    "(?x ≤ ?y) ||> bounded_minus(x, y) >> 0",

    "(?xs take 0) >> ⟨⟩",
    "(?xs take (len xs)) >> id xs",
    "((?xs ++ ?xs') take ?x) >> ((xs take (min len(xs) x)) ++ (xs' take (bounded_minus x len xs)))",

    // merge range
    "(range_exclude(?x, ?y) ++ range_exclude(y, ?z)) >> range_exclude(x, z)",
    // exclude to include
    "range_exclude(?x, ?y + 1) = range_include(x, y)",
    // singleton range
    "range_include(?x, x) = (x :: ⟨⟩)",
    "(?z ∈ range_exclude(?x, ?y) ||| true) >> ((x ≤ z) ||| (z < y))"
  )

  override protected val ruleTemplates: Set[Term] = templates.map(parser.apply)

  override protected def metadata: Metadata = EmptyMetadata
}

object SimpleRewriteRulesDB {
  def apply(): SimpleRewriteRulesDB = new SimpleRewriteRulesDB()
}

object AssociativeRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z).map(_.root)

  private val parser = new TranscalParser

  override protected val ruleTemplates: Set[Term] = Set(
    "(?x ∧ (?y ∧ ?z)) = ((x ∧ y) ∧ z)",
    "?x ++ (?y ++ ?z) = (x ++ y) ++ z",
    "(?x :: ?xs) ++ ?xs' = (x :: (xs ++ xs'))",
    "(?x + (?y + ?z)) = ((x + y) + z)"
  ).map(t => parser.apply(t))

  override protected def metadata: Metadata = AssociativeMetadata

  case object AssociativeMetadata extends Metadata {
    override def toStr: String = "AssociativeMetadata"
  }

}

object ExistentialRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(xs, exist).map(_.root)

  override protected val ruleTemplates: Set[Term] = Set(
    xs =:> ((xs take exist) ++ (xs drop exist))
  )

  override protected def metadata: Metadata = ExistentialMetadata

  case object ExistentialMetadata extends Metadata {
    override def toStr: String = "ExistentialMetadata"
  }

}
