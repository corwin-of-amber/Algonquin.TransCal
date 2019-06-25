package synthesis

import com.typesafe.scalalogging.LazyLogging
import structures.{EmptyMetadata, Metadata}
import synthesis.actions.operators.LetAction
import synthesis.rewrites.{FlattenRewrite, RewriteRule, RewriteSearchState}
import synthesis.search.Operator
import transcallang.{AnnotatedTree, TranscalParser}

/**
  * @author tomer
  * @since 12/27/18
  */
trait RewriteRulesDB extends LazyLogging {
  protected def ruleTemplates: Set[AnnotatedTree]

  protected def metadata: Metadata

  private def ruleTemplatesToRewriteRules(ruleTemplate: AnnotatedTree): Set[RewriteRule] = new LetAction(ruleTemplate).rules

  lazy val rewriteRules: Set[Operator[RewriteSearchState]] = ruleTemplates.flatMap(ruleTemplatesToRewriteRules)
}

object SystemRewriteRulesDB extends RewriteRulesDB {
  override lazy val rewriteRules: Set[Operator[RewriteSearchState]] = Set[Operator[RewriteSearchState]](FlattenRewrite)

  override protected def ruleTemplates: Set[AnnotatedTree] = throw new NotImplementedError()

  override protected def metadata: Metadata = throw new NotImplementedError()
}

object SimpleRewriteRulesDB extends RewriteRulesDB {
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
    "?x + ?y = y + x",

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
//
//    // merge range
    "(range_exclude(?x, ?y) ++ range_exclude(y, ?z)) >> range_exclude(x, z)",
    // exclude to include
    "range_exclude(?x, ?y + 1) = range_include(x, y)",
//    // singleton range
    "range_include(?x, x) = (x :: ⟨⟩)",
    "(?z ∈ range_exclude(?x, ?y) ||| true) >> ((x ≤ z) ||| (z < y))"
  )

  override protected val ruleTemplates: Set[AnnotatedTree] = templates.map(parser.apply)
}

object AssociativeRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  override protected def metadata: Metadata = AssociativeMetadata

  private case object AssociativeMetadata extends Metadata {
    override def toStr: String = "AssociativeMetadata"
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    "(?x ∧ (?y ∧ ?z)) = ((x ∧ y) ∧ z)",
    "?x ++ (?y ++ ?z) = (x ++ y) ++ z",
    "(?x :: ?xs) ++ ?xs' = (x :: (xs ++ xs'))",
    "(?x + (?y + ?z)) = ((x + y) + z)"
  ).map(t => parser.apply(t))

}

object TimeComplexRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  override protected def metadata: Metadata = TimeComplexMetadata

  private case object TimeComplexMetadata extends Metadata {
    override def toStr: String = "TimeComplexMetadata"
  }

  private def stringifyOperatorBinary(operator: String) = {
    "(timecomplex ?x ?v) |||| (timecomplex ?x' ?u) |||| " +
      f"(x $operator x') |>> timecomplex (x $operator x') (v + u + 1) ||| timecomplexTrue"
  }

  private def stringifyOperatorBinaryRightList(operator: String) = {
    "(timecomplex ?x ?v) |||| (timecomplex ?xs ?u) |||| (spacecomplex xs ?w) |||| " +
      f"(x $operator xs) |>> timecomplex (x $operator xs) (v + u + w + 1) ||| timecomplexTrue"
  }

  private def stringListUnary(function: String) = {
    "(timecomplex ?xs ?v) |||| (spacecomplex xs ?w) |||| " +
      f"($function ?xs) |>> (timecomplex ($function xs) (w + v + 1)) ||| timecomplexTrue"
  }

  private def stringListOperatorBinary(operator: String) = {
    "(timecomplex ?xs ?v) |||| (spacecomplex xs ?w) |||| (timecomplex ?xs' ?u) |||| (spacecomplex xs' ?x) |||| " +
      f"(ys $operator xs') |>> (timecomplex (ys $operator ?xs) (w + v + u + x + 1)) ||| timecomplexTrue"
  }

  private def stringListFunctionBinary(function: String) = {
    "(timecomplex ?xs ?v) |||| (spacecomplex xs ?w) |||| (timecomplex ?xs' ?u) |||| (spacecomplex xs' ?x) |||| " +
      f"($function xs xs') |>> (timecomplex ($function xs xs') (w + v + u + x + 1)) ||| timecomplexTrue"
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    stringListFunctionBinary("elem"),
    stringListOperatorBinary("∪"),
    stringListOperatorBinary("‖"),
    stringListUnary("elems"),
//    stringListUnary("len"),
    "(timecomplex (?x) ?u) |||| (~x) |>> timecomplex (~x) (u + 1) ||| timecomplexTrue",
//    "(timecomplex (~(?x)) ?u) |>> timecomplex (x) (u + 1) ||| timecomplexTrue",
    "(timecomplex (?x) ?u) |||| ({x}) |>> timecomplex ({x}) (u + 1) ||| timecomplexTrue",
    stringifyOperatorBinary("=="),
    stringifyOperatorBinary("∧"),
    stringifyOperatorBinary("∨"),
    stringifyOperatorBinary("≠"),
    stringifyOperatorBinaryRightList("::"),
    stringifyOperatorBinaryRightList("∈"),
    stringifyOperatorBinaryRightList("∉")
  ).map(t => parser.apply(t))

}

object ExistentialRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  override protected def metadata: Metadata = ExistentialMetadata

  private case object ExistentialMetadata extends Metadata {
    override def toStr: String = "ExistentialMetadata"
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    "?xs = ((xs take ?exist) ++ (xs drop exist))"
  ).map(t => parser.apply(t))
}
