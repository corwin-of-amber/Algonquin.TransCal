package relentless

import com.typesafe.scalalogging.LazyLogging
import relentless.RewriteRule.RuleType
import relentless.matching.{Encoding, Trie}
import relentless.rewriting.Rewriter
import syntax.AstSugar._
import syntax.Formula.O
import syntax.{Formula, Identifier, Scheme, Tree}


object BasicSignature {

  val `⇒` = I("⇒", "operator") // used for guarded command, instead of |! that we used in Bellmania
  def `⇒:`(x: Term, y: Term): Term = T(`⇒`, List(x, y))

  val tt = TV("⊤")
  val ff = TV("⊥")

  val _id = TV("id")

  def id(x: Term) = _id :@ (x)

  val x = TV("x");
  val y = TV("y");
  val z = TV("z");
  val w = TV("w");
  val v = TV("v")
  val `x'` = TV("x'")
  val xs = TV("xs");
  val `xs'` = TV("xs'")
  val xss = TV("xss")

  val f = TV("f")
  val l = TV("l")

  val _nil = TV("⟨⟩")
  val _cons = TV("::")
  val _elem = TV("elem")
  val _elems = TI("elems")

  val `_++` = TV("++")
  val _take = TV("take")
  val _drop = TV("drop")

  val _ne = TI("≠")
  val _in = TI("∈")
  val _not_in = TI("∉")
  val _set_singleton = TI("{.}")
  val _set_disj = TI("‖")
  val _set_union = TI("∪")

  def `!=:=`(x: Term, y: Term) = _ne :@ (x, y)

  def in(x: Term, xs: Term) = _in :@ (x, xs)

  def not_in(x: Term, xs: Term) = _not_in :@ (x, xs)

  def `{}`(x: Term) = _set_singleton :@ (x)

  def set_disj(s: Term, t: Term) = _set_disj :@ (s, t)

  def set_union(s: Term, t: Term) = _set_union :@ (s, t)

  def nil = _nil

  def cons(x: Term, xs: Term) = _cons :@ (x, xs)

  def elem(x: Term, xs: Term) = _elem :@ (x, xs)

  def elems(xs: Term) = _elems :@ (xs)

  def ++(x: Term, y: Term) = `_++` :@ (x, y)

  def take(xs: Term, y: Term) = _take :@ (xs, y)

  def drop(xs: Term, y: Term) = _drop :@ (xs, y)

  class Brackets(left: String, right: String) extends Formula.Notation {

    import Formula._
    import report.data.TapeString._

    def format(term: Term) = {
      tape"${left}${display(term.subtrees.head)}${right}"
    }

    val precedence = 0
    override val arity = 1
  }

  import Formula.{M, O}

  Formula.INFIX ++= M(O("≠", 5), O("‖", 5), O("∈", 5), O("∉", 5), O("∪", 5), O("++", 5), O("⇒", 5)) + ("{.}" -> new Brackets("{", "}"))
}


trait Rules extends LazyLogging {
  lazy implicit val directory = {
    def D(root: Trie.DirectoryEntry, subtrees: Tree[Trie.DirectoryEntry]*) = new Tree(root, subtrees.toList)
    D(-1, D(0, D(1, D(2, D(3, D(4))), D(3)), D(2, D(3, D(4))), D(3)))
  }

  implicit val enc: Encoding

  val vars: List[Term]
  val rulesSrc: List[RewriteRule]
  val ruleTemplates: List[Tree[Identifier]]

  lazy val rules = Rewriter.compileRules(rulesSrc)

  def templatesToRewriteRules(ruleType: RuleType) : List[RewriteRule] = {
    ruleTemplates zip Stream.continually(vars) flatMap ((t: Tree[Identifier], v: List[Tree[Identifier]]) =>
      RewriteRule.apply(t, v, ruleType)).tupled
  }
}

object RewriteRule extends Enumeration {
  type RuleType = Value
  val Basic, Associative, Goal, Locator, Definition, Existential = Value

  val `=>` = I("=>", "operator") // directional rewrite
  val ||| = I("|||", "operator") // parallel patterns or conclusions
  val ||> = I("||>", "operator")

  implicit class RuleOps(private val t: Term) extends AnyVal {
    def =:>(s: Term): Tree[Identifier] = T(`=>`)(t, s)

    def |||(s: Term): Tree[Identifier] = T(RewriteRule.|||)(t, s)

    def ||>(s: Term): Tree[Identifier] = T(RewriteRule.||>)(t, s)
  }

  Formula.INFIX ++= List(`=>` -> O("=>", 5), `|||` -> O("|||", 5))

  def apply(template: Scheme.Template, ruleType: RewriteRule.RuleType): List[RewriteRule] = {
    RewriteRule(template.template, template.vars map (T(_)), ruleType)
  }

  def apply(rule: Tree[Identifier], vars: List[Tree[Identifier]], ruleType: RewriteRule.RuleType): List[RewriteRule] = {
    def varsUsed(t: Term) = vars filter t.leaves.contains

    rule match {
      case eqn@T(`=>`, List(lhs, rhs)) =>
        val v = varsUsed(eqn) map (_.leaf)
        List(new RewriteRule(new Scheme.Template(v, lhs), new Scheme.Template(v, rhs), ruleType))
      case eqn@T(`=`, List(lhs, rhs)) =>
        val v = varsUsed(eqn) map (_.leaf)
        val (l, r) = (new Scheme.Template(v, lhs), new Scheme.Template(v, rhs))
        List(new RewriteRule(l, r, ruleType), new RewriteRule(r, l, ruleType))
      case other =>
        throw new RuntimeException(s"invalid syntax for rule: ${other toPretty}")
    }
  }
}

class RewriteRule(val src: Scheme.Template, val target: Scheme.Template, val ruleType: RewriteRule.RuleType) { }

class BasicRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._
  import RewriteRule.RuleOps

  private val assocRules = new AssocRules()

  val vars = List(x, y, z, `x'`, xs, `xs'`)

  val ruleTemplates = List(
    (`⇒:`(tt, y)) =:> id(y),
    (`⇒:`(ff, y)) =:> ff,
    ~tt =:= ff,
    ~ff =:= tt,
    (x /: ff) =:> id(x),
    (ff /: x) =:> id(x),
    id(id(x)) =:> id(x),

    (x =:= `x'`) =:= (in(`x'`, `{}`(x))),
    elem(x, cons(`x'`, `xs'`)) =:= ((x =:= `x'`) | elem(x, `xs'`)),
    ~(x =:= y) =:= `!=:=`(x, y),
    ~(in(x, y)) =:= not_in(x, y),
    not_in(x, xs) =:= set_disj(`{}`(x), xs),
    set_disj(xs, `{}`(x)) =:> not_in(x, xs),
    ~(x | y) =:= (~x & ~y),
    ~(x & y) =:= (~x | ~y),
    (set_disj(x, xs) & set_disj(y, xs)) =:= (set_disj(set_union(x, y), xs)),
    (set_disj(xs, x) & set_disj(xs, y)) =:= (set_disj(xs, set_union(x, y))),
    elem(x, xs) =:= in(x, elems(xs)),
    elems(cons(`x'`, `xs'`)) =:= (set_union(`{}`(`x'`), elems(`xs'`))), // <-- this one is somewhat superfluous?
    ++(cons(x, xs), `xs'`) =:= cons(x, ++(xs, `xs'`))
  )

  val rulesSrc : List[RewriteRule] = assocRules.rulesSrc ++ templatesToRewriteRules(RewriteRule.Basic)
}

class AssocRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._

  val vars = List(x, y, z, `x'`, xs, `xs'`)

  override val ruleTemplates: List[Tree[Identifier]] = List((x & (y & z)) =:= (x & y & z))

  val rulesSrc = templatesToRewriteRules(RewriteRule.Associative)
}

class ExistentialRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._
  import RewriteRule.RuleOps

  private val basicRules = new BasicRules()

  val vars = List(x, y, z, `x'`, xs, `xs'`)

  val ruleTemplates = List(
    xs =:> ++(take(xs, y), drop(xs, y))
  )

  val rulesSrc : List[RewriteRule] = basicRules.rulesSrc ++ templatesToRewriteRules(RewriteRule.Existential)
}