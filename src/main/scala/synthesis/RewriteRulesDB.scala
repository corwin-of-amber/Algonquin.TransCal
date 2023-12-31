package synthesis

import com.typesafe.scalalogging.LazyLogging
import structures.immutable.HyperGraph
import structures.{EmptyMetadata, HyperEdge, Ignored, Metadata}
import synthesis.actions.operators.LetAction
import synthesis.rewrites.Template.{ExplicitTerm, ReferenceTerm}
import synthesis.rewrites._
import synthesis.search.Operator
import transcallang.{AnnotatedTree, Language, TranscalParser}

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

  val ADD_TIME_COMPLEX = "addt"

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
    val parameters = whatIsConstant.indices.map("x" + "x"*_)
    val (firstCall, call) = if (isFunction) {
      (f"($functionName ${parameters.map("?"+_).mkString(" ")})", f"($functionName ${parameters.mkString(" ")})")
    } else {
      assert(whatIsConstant.size == 2)
      (f"(?${parameters.head} $functionName ?${parameters(1)})", f"(${parameters.head} $functionName ${parameters(1)})")
    }

    val complexitiesWithNames = whatIsConstant.zip(parameters).flatMap({
      case (isConstant, parameterName) =>
        Seq({
          val tcParameter = "tc" + parameterName
          (tcParameter, f"(timecomplex $parameterName ?$tcParameter)")
        }) ++ (if (isConstant) None else Some({
          val scParameter = "sc" + parameterName
          (scParameter, f"(${Language.spaceComplexId.literal} $parameterName ?$scParameter)")
        }))
    })
    val complexities = complexitiesWithNames.map(_._2)
    val names = complexitiesWithNames.map(_._1)
    val premise = (firstCall +: complexities).mkString(" |||| ")
    val conclusion = f"timecomplex $call (${("1" +: names).reduce((a, b) => f"$ADD_TIME_COMPLEX($a, $b)")}) ||| timecomplexTrue"
    premise + " |>> " + conclusion
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    buildFunction("elem", Seq(true, false)),
    buildOperator(Language.plusId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.unionId.literal, isFirstConstant = false, isSecondConstant = false),
    buildOperator(Language.setDisjointId.literal, isFirstConstant = false, isSecondConstant = false),
    buildUnaryFunction("elems", isConstant = false),
    buildUnaryFunction("len", isConstant = true),
    buildUnaryFunction("~", isConstant = true),
//    "(timecomplex (~(?x)) ?u) |>> timecomplex (x) (u + 1) ||| timecomplexTrue",
    f"{?x} |||| (${Language.timeComplexId.literal} x ?tcx) |>> ${Language.timeComplexId.literal} {x} ($ADD_TIME_COMPLEX(1, tcx)) ||| ${Language.timeComplexTrueId.literal}",
    buildOperator(Language.equalityId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.andId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.orId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.unequalityId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.consId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.setContainsId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.setNotContainsId.literal, isFirstConstant = true, isSecondConstant = true),
  ).map(t => parser.apply(t))

  private def ruleTemplatesToRewriteRules(ruleTemplate: AnnotatedTree): Set[RewriteRule] = new LetAction(ruleTemplate).rules
  private val guardedTimeComplexRewriteRule = new RewriteRule(
    HyperGraph(
      HyperEdge(ReferenceTerm(0), ExplicitTerm(HyperTermIdentifier(Language.guardedId)), Seq(Ignored(), ReferenceTerm(1)), EmptyMetadata),
      HyperEdge(ReferenceTerm(2), ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(ReferenceTerm(1), ReferenceTerm(3)), EmptyMetadata),
      HyperEdge(ReferenceTerm(2), ExplicitTerm(HyperTermIdentifier(Language.timeComplexTrueId)), Seq.empty, EmptyMetadata)
    ),
    HyperGraph(
      HyperEdge(ReferenceTerm(2), ExplicitTerm(HyperTermIdentifier(Language.timeComplexId)), Seq(ReferenceTerm(0), ReferenceTerm(3)), EmptyMetadata)
    ),
    (_, _) => TimeComplexMetadata
  )
  override lazy val rewriteRules = ruleTemplates.flatMap(ruleTemplatesToRewriteRules) ++ Seq(
    guardedTimeComplexRewriteRule, MatchTimeComplexRewrite, TupleTimeComplexRewrite
  )

}

object SpaceComplexRewriteRulesDB extends RewriteRulesDB {
  private val parser = new TranscalParser

  val ADD_SPACE_COMPLEX = "adds"

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
    val parameters = whatIsConstant.indices.map("x" + "x"*_)
    val (firstCall, call) = if (isFunction) {
      (f"($functionName ${parameters.map("?"+_).mkString(" ")})", f"($functionName ${parameters.mkString(" ")})")
    } else {
      assert(whatIsConstant.size == 2)
      (f"(?${parameters.head} $functionName ?${parameters(1)})", f"(${parameters.head} $functionName ${parameters(1)})")
    }

    val complexitiesWithNames = whatIsConstant.zip(parameters).map({
      case (isConstant, parameterName) =>
        val scParameter = if (isConstant) "1" else "sc" + parameterName
        (scParameter, f"(${Language.spaceComplexId.literal} $parameterName ?$scParameter)")
    })
    val complexities = complexitiesWithNames.map(_._2)
    val names = complexitiesWithNames.map(_._1)
    val premise = (firstCall +: complexities).mkString(" |||| ")
    val conclusion = f"${Language.spaceComplexId.literal} $call (${names.reduce((a,b) => f"$ADD_SPACE_COMPLEX($a, $b)")}) ||| ${Language.spaceComplexTrueId.literal}"
    premise + " |>> " + conclusion
  }

  override protected val ruleTemplates: Set[AnnotatedTree] = Set(
    f"{?x} |>> ${Language.spaceComplexId.literal} {x} 1 ||| ${Language.spaceComplexTrueId.literal}",
    buildUnaryFunction("elems", isConstant = false),
    buildOperator(Language.plusId.literal, isFirstConstant = true, isSecondConstant = true),
    buildOperator(Language.unionId.literal, isFirstConstant = false, isSecondConstant = false),
    buildOperator(Language.setDisjointId.literal, isFirstConstant = false, isSecondConstant = false),
    buildOperator(Language.consId.literal, isFirstConstant = true, isSecondConstant = false),
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
