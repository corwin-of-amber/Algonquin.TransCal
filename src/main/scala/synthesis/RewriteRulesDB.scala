package synthesis

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature._
import relentless.rewriting.RewriteRule._
import structures.{EmptyMetadata, Metadata}
import syntax.AstSugar._
import syntax.Identifier
import synthesis.actions.operators.LetAction
import synthesis.rewrites.{FlattenRewrite, FunctionReturnTypeRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import transcallang.TranscalParser

/**
  * @author tomer
  * @since 12/27/18
  */
trait RewriteRulesDB extends LazyLogging {
  protected def vars: Set[Identifier]

  protected def ruleTemplates: Set[Term]

  protected def metadata: Metadata

  private def ruleTemplatesToRewriteRules(ruleTemplate: Term): Set[RewriteRule] = new LetAction(ruleTemplate).rules

  lazy val rewriteRules: Set[Operator[RewriteSearchState]] = Set[Operator[RewriteSearchState]](FlattenRewrite, FunctionReturnTypeRewrite) ++ ruleTemplates.flatMap(ruleTemplatesToRewriteRules)
}

object SimpleRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z, `x'`, xs).map(_.root)

  private val parser = new TranscalParser

  override protected def metadata: Metadata = EmptyMetadata

  private val templates: Set[String] = Set(
    "~true = false",
    "~false = true",
    "id (?x) >> x",

    "?x == ?x' = x' == x",
    "?x == ?x' = x' ∈ { x }",
    "elem(?x, (?x' :: ?xs')) = ((x == x') \\/ elem(x, xs'))",
    "~(?x == ?y) = (x != y)",
    "~(?x ∈ ?y) = (x ∉ y)",
    "(?x ∉ ?xs) = { x } ‖ xs",
    "~(?x \\/ ?y) = (~x /\\ ~y)",
    "~(?x /\\ ?y) = (~x \\/ ~y)",
    "((?x ‖ ?xs) /\\ (?y ‖ xs)) = ((x ∪ y) ‖ xs)",
    "((?xs ‖ ?x) /\\ (xs ‖ ?y)) = (xs ‖ (x ∪ y))",
    "?x ∈ (?xs ∪ ?ys) = (x ∈ xs) \\/ (x ∈ ys)",
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

    "(take ?xs 0) >> ⟨⟩",
    "(take ?xs (len xs)) >> id xs",
    "(take (?xs ++ ?xs') ?x) >> ((take xs (min len(xs) x)) ++ (take xs' (bounded_minus x len xs)))",

    // merge range
    "(range_exclude(?x, ?y) ++ range_exclude(y, ?z)) >> range_exclude(x, z)",
    // exclude to include
    "range_exclude(?x, ?y + 1) = range_include(x, y)",
    // singleton range
    "range_include(?x, x) = (x :: ⟨⟩)",
    "(?z ∈ range_exclude(?x, ?y) ||| true) >> ((x ≤ z) ||| (z < y))"
  )

  override protected val ruleTemplates: Set[Term] = templates.map(parser.apply)
}

object AssociativeRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z).map(_.root)

  private val parser = new TranscalParser

  override protected def metadata: Metadata = AssociativeMetadata

  case object AssociativeMetadata extends Metadata {
    override def toStr: String = "AssociativeMetadata"
  }

  override protected val ruleTemplates: Set[Term] = Set(
    "(?x ∧ (?y ∧ ?z)) = ((x ∧ y) ∧ z)",
    "?x ++ (?y ++ ?z) = (x ++ y) ++ z",
    "(?x :: ?xs) ++ ?xs' = (x :: (xs ++ xs'))",
    "(?x + (?y + ?z)) = ((x + y) + z)"
  ).map(t => parser.apply(t))

}

object OwnershipRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(x, y, z).map(_.root)

  private val parser = new TranscalParser

  override protected def metadata: Metadata = OwnershipMetadata

  case object OwnershipMetadata extends Metadata {
    override def toStr: String = "OwnershipMetadata"
  }

  override protected val ruleTemplates: Set[Term] = Set(
    "(?x ++ ?y ||| ?z) & (own x ||| true) & (own y ||| true) ||> true = own z"
  ).map(t => parser.apply(t))

}

object ExistentialRewriteRulesDB extends RewriteRulesDB {
  override protected val vars: Set[Identifier] = Set(xs, exist).map(_.root)

  override protected def metadata: Metadata = ExistentialMetadata

  case object ExistentialMetadata extends Metadata {
    override def toStr: String = "ExistentialMetadata"
  }

  override protected val ruleTemplates: Set[Term] = Set(
    xs =:> ((xs take exist) ++ (xs drop exist))
  )
}
