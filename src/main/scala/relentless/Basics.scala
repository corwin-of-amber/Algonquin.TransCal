package relentless

import com.typesafe.scalalogging.LazyLogging
import relentless.matching.Encoding
import relentless.matching.structures.vocabulary.Vocabulary
import relentless.rewriting.RewriteRule
import relentless.rewriting.RewriteRule.Category
import report.data.TapeString
import syntax.AstSugar._
import syntax.{Formula, Identifier, Tree}


object BasicSignature {

  val `⇒`: Identifier = I("⇒", "operator") // used for guarded command, instead of |! that we used in Bellmania
  def `⇒:`(x: Term, y: Term): Term = T(`⇒`, List(x, y))

  val tt: Tree[Identifier] = TV("⊤")
  val ff: Tree[Identifier] = TV("⊥")

  val _id: Tree[Identifier] = TV("id")

  def id(x: Term): Tree[Identifier] = _id :@ x

  val x: Tree[Identifier] = TV("x")
  val y: Tree[Identifier] = TV("y")
  val z: Tree[Identifier] = TV("z")
  val w: Tree[Identifier] = TV("w")
  val v: Tree[Identifier] = TV("v")
  val `x'`: Tree[Identifier] = TV("x'")
  val xs: Tree[Identifier] = TV("xs")
  val `xs'`: Tree[Identifier] = TV("xs'")
  val xss: Tree[Identifier] = TV("xss")
  val exist: Tree[Identifier] = TV("exist")

  val f: Tree[Identifier] = TV("f")
  val l: Tree[Identifier] = TV("l")

  val _nil: Tree[Identifier] = TV("⟨⟩")
  val _zero: Tree[Identifier] = TV(0)
  val _one: Tree[Identifier] = TV(1)
  val _cons: Tree[Identifier] = TV("::")
  val _snoc: Tree[Identifier] = TV(":+")
  val _elem: Tree[Identifier] = TV("elem")
  val _elems: Tree[Identifier] = TI("elems")

  val `_++`: Tree[Identifier] = TV("++")
  val _take: Tree[Identifier] = TV("take")
  val _len: Tree[Identifier] = TV("len")
  val _drop: Tree[Identifier] = TV("drop")

  val _ne: Tree[Identifier] = TI("≠")
  val _in: Tree[Identifier] = TI("∈")
  val _not_in: Tree[Identifier] = TI("∉")
  val _set_singleton: Tree[Identifier] = TI("{.}")
  val _set_disj: Tree[Identifier] = TI("‖")
  val _set_union: Tree[Identifier] = TI("∪")

  val _lt: Tree[Identifier] = TI("<")
  val _gt: Tree[Identifier] = TI(">")
  val _le: Tree[Identifier] = TI("≤")
  val _ge: Tree[Identifier] = TI("≥")

  val _min: Tree[Identifier] = TI("min")
  val _bounded_minus: Tree[Identifier] = TI("bounded_minus")
  val _max: Tree[Identifier] = TI("max")

  val _range_exclude: Tree[Identifier] = TI("range_exclude")
  val _range_include: Tree[Identifier] = TI("range_include")

  def `!=:=`(x: Term, y: Term): Tree[Identifier] = _ne :@ (x, y)

  def in(x: Term, xs: Term): Tree[Identifier] = _in :@ (x, xs)

  def range_exclude(x: Term, y: Term): Tree[Identifier] = _range_exclude :@ (x, y)
  def range_include(x: Term, y: Term): Tree[Identifier] = _range_include :@ (x, y)

  def not_in(x: Term, xs: Term): Tree[Identifier] = _not_in :@ (x, xs)

  def `{}`(x: Term): Tree[Identifier] = _set_singleton :@ x

  def set_disj(s: Term, t: Term): Tree[Identifier] = _set_disj :@ (s, t)

  def set_union(s: Term, t: Term): Tree[Identifier] = _set_union :@ (s, t)

  def nil: Tree[Identifier] = _nil
  def one: Tree[Identifier] = _one
  def zero: Tree[Identifier] = _zero

  def cons(x: Term, xs: Term): Tree[Identifier] = _cons :@ (x, xs)

  def snoc(x: Term, xs: Term): Tree[Identifier] = _snoc:@(x, xs)

  def elem(x: Term, xs: Term): Tree[Identifier] = _elem :@ (x, xs)

  def elems(xs: Term): Tree[Identifier] = _elems :@ xs

  def ++(x: Term, y: Term): Tree[Identifier] = `_++` :@ (x, y)

  def take(xs: Term, y: Term): Tree[Identifier] = _take :@ (xs, y)
  def len(xs: Term): Tree[Identifier] = _len :@ xs

  def drop(xs: Term, y: Term): Tree[Identifier] = _drop :@ (xs, y)

  def bounded_minus(x: Term, y: Term): Tree[Identifier] = _bounded_minus :@ (x, y)
  def min(x: Term, y: Term): Tree[Identifier] = _min :@ (x, y)
  def max(x: Term, y: Term): Tree[Identifier] = _max :@ (x, y)

  def <(x: Term, y: Term): Tree[Identifier] = _lt :@ (x, y)
  def >(x: Term, y: Term): Tree[Identifier] = _gt :@ (x, y)
  def ≤(x: Term, y: Term): Tree[Identifier] = _le :@ (x, y)
  def ≥(x: Term, y: Term): Tree[Identifier] = _ge :@ (x, y)

  implicit class ListDSL(private val term: Term) extends AnyVal {
    def elem(that: Term) = BasicSignature.elem(term, that)
    def ++(that: Term) = BasicSignature.++(term, that)
    def cons(that: Term) = BasicSignature.cons(term, that)
    def snoc(that: Term) = BasicSignature.snoc(term, that)
    def take(that: Term) = BasicSignature.take(term, that)
    def drop(that: Term) = BasicSignature.drop(term, that)
  }
  implicit class NumberDSL(private val term: Term) extends AnyVal {
    def ≤(that: Term) = BasicSignature.≤(term, that)
    def ≥(that: Term) = BasicSignature.≥(term, that)
  }
  class Brackets(left: String, right: String) extends Formula.Notation {

    import Formula._
    import report.data.TapeString._

    def format(term: Term): TapeString = {
      tape"$left${display(term.subtrees.head)}$right"
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
  lazy implicit val directory: Tree[Vocabulary.DirectoryEntry] = {
    def D(root: Vocabulary.DirectoryEntry, subtrees: Tree[Vocabulary.DirectoryEntry]*) = new Tree(root, subtrees.toList)
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
    `⇒:`(tt, y) =:> id(y),
    `⇒:`(ff, y) =:> ff,
    ~tt =:= ff,
    ~ff =:= tt,
    (x /: ff) =:> id(x),
    (ff /: x) =:> id(x),
    id(id(x)) =:> id(x),

    (x =:= `x'`) =:= in(`x'`, `{}`(x)),
    elem(x, cons(`x'`, `xs'`)) =:= ((x =:= `x'`) | elem(x, `xs'`)),
    ~(x =:= y) =:= `!=:=`(x, y),
    ~in(x, y) =:= not_in(x, y),
    not_in(x, xs) =:= set_disj(`{}`(x), xs),
    set_disj(xs, `{}`(x)) =:> not_in(x, xs),
    ~(x | y) =:= (~x & ~y),
    ~(x & y) =:= (~x | ~y),
    (set_disj(x, xs) & set_disj(y, xs)) =:= set_disj(set_union(x, y), xs),
    (set_disj(xs, x) & set_disj(xs, y)) =:= set_disj(xs, set_union(x, y)),
    elem(x, xs) =:= in(x, elems(xs)),
    elems(cons(`x'`, `xs'`)) =:= set_union(`{}`(`x'`), elems(`xs'`)), // <-- this one is somewhat superfluous?

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

    take(xs, zero) =:> nil,
    take(xs, len(xs)) =:> xs,
    take(++(xs, `xs'`), x) =:> ++(take(xs, min(len(xs), x)), take(`xs'`, bounded_minus(x, len(xs)))),

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

  val rules: List[RewriteRule] = templatesToRewriteRules(RewriteRule.Category.Associative)
}

class ExistentialRules(implicit val enc: Encoding) extends Rules {

  import BasicSignature._
  import RewriteRule.RuleOps

  private val basicRules = new BasicRules()

  val vars = List(x, y, z, `x'`, `xs'`, exist)

  // TODO: return xs to vars after we have types.
  val ruleTemplates = List(
    xs =:> ++(take(xs, exist), drop(xs, exist))
  )

  val rules : List[RewriteRule] = basicRules.rules ++ templatesToRewriteRules(RewriteRule.Category.Existential)
}
