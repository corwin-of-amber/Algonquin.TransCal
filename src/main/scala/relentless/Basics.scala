package relentless

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.{Encoding, Trie}
import relentless.rewriting.RewriteRule
import relentless.rewriting.RewriteRule.Category
import syntax.AstSugar._
import syntax.{Formula, Identifier, Tree}


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
  val exist = TV("exist")

  val f = TV("f")
  val l = TV("l")

  val _nil = TV("⟨⟩")
  val _zero = TV(0)
  val _one = TV(1)
  val _cons = TV("::")
  val _snoc = TV(":+")
  val _elem = TV("elem")
  val _elems = TI("elems")

  val `_++` = TV("++")
  val _take = TV("take")
  val _len = TV("len")
  val _drop = TV("drop")

  val _ne = TI("≠")
  val _in = TI("∈")
  val _not_in = TI("∉")
  val _set_singleton = TI("{.}")
  val _set_disj = TI("‖")
  val _set_union = TI("∪")

  val _lt = TI("<")
  val _gt = TI(">")
  val _le = TI("≤")
  val _ge = TI("≥")

  val _min = TI("min")
  val _bounded_minus = TI("bounded_minus")
  val _max = TI("max")

  val _range_exclude = TI("range_exclude")
  val _range_include = TI("range_include")

  def `!=:=`(x: Term, y: Term) = _ne :@ (x, y)

  def in(x: Term, xs: Term) = _in :@ (x, xs)

  def range_exclude(x: Term, y: Term) = _range_exclude :@ (x, y)
  def range_include(x: Term, y: Term) = _range_include :@ (x, y)

  def not_in(x: Term, xs: Term) = _not_in :@ (x, xs)

  def `{}`(x: Term) = _set_singleton :@ (x)

  def set_disj(s: Term, t: Term) = _set_disj :@ (s, t)

  def set_union(s: Term, t: Term) = _set_union :@ (s, t)

  def nil = _nil
  def one = _one
  def zero = _zero

  def cons(x: Term, xs: Term) = _cons :@ (x, xs)

  def snoc(x: Term, xs: Term) = _snoc:@(x, xs)

  def elem(x: Term, xs: Term) = _elem :@ (x, xs)

  def elems(xs: Term) = _elems :@ (xs)

  def ++(x: Term, y: Term) = `_++` :@ (x, y)

  def take(xs: Term, y: Term) = _take :@ (xs, y)
  def len(xs: Term) = _len :@ (xs)

  def drop(xs: Term, y: Term) = _drop :@ (xs, y)

  def bounded_minus(x: Term, y: Term) = _bounded_minus :@ (x, y)
  def min(x: Term, y: Term) = _min :@ (x, y)
  def max(x: Term, y: Term) = _max :@ (x, y)

  def <(x: Term, y: Term) = _lt :@ (x, y)
  def >(x: Term, y: Term) = _gt :@ (x, y)
  def ≤(x: Term, y: Term) = _le :@ (x, y)
  def ≥(x: Term, y: Term) = _ge :@ (x, y)

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

  Formula.INFIX ++= M(O("≤", 5), O("≥", 5),
    O("≠", 5), O("‖", 5), O("∈", 5),
    O("∉", 5), O("∪", 5), O("++", 5),
    O("⇒", 5)) + ("{.}" -> new Brackets("{", "}"))
}


trait Rules extends LazyLogging {
  lazy implicit val directory = {
    def D(root: Trie.DirectoryEntry, subtrees: Tree[Trie.DirectoryEntry]*) = new Tree(root, subtrees.toList)
    D(-1, D(0, D(1, D(2, D(3, D(4))), D(3)), D(2, D(3, D(4))), D(3)), D(1))
  }

  implicit val enc: Encoding

  val vars: List[Term]
  val ruleTemplates: List[Tree[Identifier]]

  def templatesToRewriteRules(ruleType: Category) : List[RewriteRule] = {
    ruleTemplates zip Stream.continually(vars) flatMap ((t: Tree[Identifier], v: List[Tree[Identifier]]) =>
      RewriteRule.apply(t, v, ruleType)).tupled
  }

  val rules: List[RewriteRule]
}


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

    snoc(y, x) =:= ++(y, cons(x, nil)),
    ++(nil, `xs'`) =:> id(`xs'`),
    ++(`xs'`, nil) =:> id(`xs'`),
    ++(x, ++(y, z)) =:= ++(++(x, y), z),
    ++(cons(x, xs), `xs'`) =:= cons(x, ++(xs, `xs'`)),

    (<(x, y) ||| tt) =:> ≤(x, y),
    ≤(x, y) ||> (min(x, y) =:> id(x)),
    ≤(x, y) ||> (min(y, x) =:> id(x)),
//    min(x, y) =:> min(y,x),

    ≤(x, y) ||> (bounded_minus(x, y) =:> zero),

    // merge range
    ++(range_exclude(x,y), range_exclude(y, z)) =:> range_exclude(x, z),
    // exclude to include
    range_exclude(x, y + one) =:= range_include(x, y),
    // singleton range
    range_include(x, x) =:= cons(x, nil),
    (in(z, range_exclude(x, y)) ||| tt) =:> (≤(x, z) ||| <(z, y))
  )

  val rules : List[RewriteRule] = assocRules.rules ++ templatesToRewriteRules(RewriteRule.Category.Basic)
}

class AssocRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._

  val vars = List(x, y, z, `x'`, xs, `xs'`)

  override val ruleTemplates: List[Tree[Identifier]] = List((x & (y & z)) =:= (x & y & z))

  val rules = templatesToRewriteRules(RewriteRule.Category.Associative)
}

class ExistentialRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._
  import RewriteRule.RuleOps

  private val basicRules = new BasicRules()

  val vars = List(x, y, z, `x'`, xs, `xs'`, exist)

  val ruleTemplates = List(
    xs =:> ++(take(xs, exist), drop(xs, exist))
  )

  val rules : List[RewriteRule] = basicRules.rules ++ templatesToRewriteRules(RewriteRule.Category.Existential)
}
