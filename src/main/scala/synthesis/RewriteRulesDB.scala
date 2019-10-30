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
//    "id (?x) >> x",

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
    "?x + 0 = id x",

    "(?y :+ ?x) = (y ++ (x :: ⟨⟩))",
    "(?x :: ?xs) ++ ?ys = (x :: (xs ++ ys))",
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

  /** Builds a time complex rule for an operator.
    *
    * @param operatorName The function name.
    * @param isFirstConstant Is the first parameter a constant.
    * @param isSecondConstant Is the second parameter a constant.
    * @return
    */
  private def buildOperator(operatorName: String, isFirstConstant: Boolean, isSecondConstant: Boolean): String =
    build(operatorName, isFunction = false, Seq(isFirstConstant, isSecondConstant))

  /** Builds a time complex rule
    *
    * @param functionName The function name.
    * @param whatIsConstant What is constant to the function, by arity order (also defines the arity).
    * @return
    */
  private def buildFunction(functionName: String, whatIsConstant: Seq[Boolean]): String =
    build(functionName, isFunction = true, whatIsConstant)

  /** Builds a time complex rule for an unary function.
    *
    * @param functionName The function name.
    * @param isConstant Is the parameter a constant.
    * @return
    */
  private def buildUnaryFunction(functionName: String, isConstant: Boolean): String =
    buildFunction(functionName, Seq(isConstant))

  /** Builds a time complex rule.
    *
    * @param functionName The function name.
    * @param isFunction If true, it's a function, otherwise a binary operator.
    * @param whatIsConstant What is constant to the function, by arity order (also defines the arity).
    * @return
    */
  private def build(functionName: String, isFunction: Boolean, whatIsConstant: Seq[Boolean]): String = {
    require(isFunction || whatIsConstant.size == 2)
    val parameters = whatIsConstant.indices.map("x" + "x"*_)
    val (firstCall, call) = if (isFunction) {
      (f"($functionName ${parameters.map("?"+_).mkString(" ")})", f"($functionName ${parameters.mkString(" ")})")
    } else {
      (f"(?${parameters.head} $functionName ?${parameters(1)})", f"(${parameters.head} $functionName ${parameters(1)})")
    }

    val complexitiesWithNames = whatIsConstant.zip(parameters).flatMap({
      case (isConstant, parameterName) =>
        Seq({
          val tcParameter = "tc" + parameterName
          (tcParameter, f"(timecomplex $parameterName ?$tcParameter)")
        }) ++ (if (isConstant) None else Some({
          val scParameter = "sc" + parameterName
          (scParameter, f"(spacecomplex $parameterName ?$scParameter)")
        }))
    })
    val complexities = complexitiesWithNames.map(_._2)
    val names = complexitiesWithNames.map(_._1)
    val premise = (firstCall +: complexities).mkString(" |||| ")
    val conclusion = f"timecomplex $call (${("1" +: names).mkString(" + ")}) ||| timecomplexTrue"
    premise + " |>> " + conclusion
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    buildFunction("elem", Seq(true, false)),
    buildOperator("∪", isFirstConstant = false, isSecondConstant = false),
    buildOperator("‖", isFirstConstant = false, isSecondConstant = false),
    buildUnaryFunction("elems", isConstant = false),
    buildUnaryFunction("len", isConstant = true),
    buildUnaryFunction("~", isConstant = true),
//    "(timecomplex (~(?x)) ?u) |>> timecomplex (x) (u + 1) ||| timecomplexTrue",
    "{?x} |||| (timecomplex x ?tcx) |>> timecomplex {x} (1 + tcx) ||| timecomplexTrue",
    buildOperator("==", isFirstConstant = true, isSecondConstant = true),
    buildOperator("∧", isFirstConstant = true, isSecondConstant = true),
    buildOperator("∨", isFirstConstant = true, isSecondConstant = true),
    buildOperator("≠", isFirstConstant = true, isSecondConstant = true),
    buildOperator("::", isFirstConstant = true, isSecondConstant = true),
    buildOperator("∈", isFirstConstant = true, isSecondConstant = true),
    buildOperator("∉", isFirstConstant = true, isSecondConstant = true),
  ).map(t => parser.apply(t))

}

object SpaceComplexRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  override protected def metadata: Metadata = SpaceComplexMetadata

  private case object SpaceComplexMetadata extends Metadata {
    override def toStr: String = "SpaceComplexMetadata"
  }

  /** Builds a time complex rule for an operator.
    *
    * @param operatorName The function name.
    * @param isFirstConstant Is the first parameter a constant.
    * @param isSecondConstant Is the second parameter a constant.
    * @return
    */
  private def buildOperator(operatorName: String, isFirstConstant: Boolean, isSecondConstant: Boolean): String =
    build(operatorName, isFunction = false, Seq(isFirstConstant, isSecondConstant))

  /** Builds a time complex rule
    *
    * @param functionName The function name.
    * @param whatIsConstant What is constant to the function, by arity order (also defines the arity).
    * @return
    */
  private def buildFunction(functionName: String, whatIsConstant: Seq[Boolean]): String =
    build(functionName, isFunction = true, whatIsConstant)

  /** Builds a time complex rule for an unary function.
    *
    * @param functionName The function name.
    * @param isConstant Is the parameter a constant.
    * @return
    */
  private def buildUnaryFunction(functionName: String, isConstant: Boolean): String =
    buildFunction(functionName, Seq(isConstant))

  /** Builds a time complex rule.
    *
    * @param functionName The function name.
    * @param isFunction If true, it's a function, otherwise a binary operator.
    * @param whatIsConstant What is constant to the function, by arity order (also defines the arity).
    * @return
    */
  private def build(functionName: String, isFunction: Boolean, whatIsConstant: Seq[Boolean]): String = {
    require(isFunction || whatIsConstant.size == 2)
    val parameters = whatIsConstant.indices.map("x" + "x"*_)
    val (firstCall, call) = if (isFunction) {
      (f"($functionName ${parameters.map("?"+_).mkString(" ")})", f"($functionName ${parameters.mkString(" ")})")
    } else {
      (f"(?${parameters.head} $functionName ?${parameters(1)})", f"(${parameters.head} $functionName ${parameters(1)})")
    }

    val complexitiesWithNames = whatIsConstant.zip(parameters).map({
      case (isConstant, parameterName) =>
        val scParameter = if (isConstant) "1" else "sc" + parameterName
        (scParameter, f"(spacecomplex $parameterName ?$scParameter)")
    })
    val complexities = complexitiesWithNames.map(_._2)
    val names = complexitiesWithNames.map(_._1)
    val premise = (firstCall +: complexities).mkString(" |||| ")
    val conclusion = f"spacecomplex $call (${names.mkString(" + ")}) ||| spacecomplexTrue"
    premise + " |>> " + conclusion
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    "{?x} |>> spacecomplex {x} 1 ||| spacecomplexTrue",
    buildUnaryFunction("elems", isConstant = false),
    buildOperator("∪", isFirstConstant = false, isSecondConstant = false),
    buildOperator("‖", isFirstConstant = false, isSecondConstant = false),
    buildOperator("::", isFirstConstant = true, isSecondConstant = false),
  ).map(t => parser.apply(t))

}

object ExistentialRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  override protected def metadata: Metadata = ExistentialMetadata

  private case object ExistentialMetadata extends Metadata {
    override def toStr: String = "ExistentialMetadata"
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    "range_exclude(?x, ?z) = range_exclude(x, ?y) ++ range_exclude(y, z)",
    "range_include(?x, len(?ys :+ ?y)) = range_include(x, len(ys)) ++ range_include(len(ys :+ y), len(ys :+ y))",
    "map ?f (?xs ++ ?ys) = (map f xs) ++ (map f ys)",
    "?xs = ((xs take ?exist) ++ (xs drop exist))"
  ).map(t => parser.apply(t))
}
