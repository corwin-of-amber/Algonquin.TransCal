package examples

import com.typesafe.scalalogging.slf4j.LazyLogging
import syntax.AstSugar._
import syntax.Formula
import relentless.rewriting.Rewrite
import relentless.matching.Encoding



object BasicSignature {
  
  val `⇒` = I("⇒", "operator")  // used for guarded command, instead of |! that we used in Bellmania
  def `⇒:`(x: Term, y: Term): Term = T(`⇒`, List(x, y))
  
  val tt = TV("⊤")
  val ff = TV("⊥")
  
  val _id = TV("id")
  def id(x: Term) = _id:@(x)
  
  val x = TV("x"); val y = TV("y"); val z = TV("z"); val w = TV("w"); val v = TV("v")
  val `x'` = TV("x'")
  val xs = TV("xs"); val `xs'` = TV("xs'")
  val xss = TV("xss")
  
  val f = TV("f")
  val l = TV("l")
  
  val _nil = TV("⟨⟩")
  val _cons = TV("::")
  val _elem = TV("elem")
  val _elems = TI("elems")

  val `_++` = TV("++")
  
  val _ne = TI("≠")
  val _in = TI("∈")
  val _not_in = TI("∉")
  val _set_singleton = TI("{.}")
  val _set_disj = TI("‖")
  val _set_union = TI("∪")
  
  def `!=:=`(x: Term, y: Term) = _ne:@(x, y)
  def in(x: Term, xs: Term) = _in:@(x, xs)
  def not_in(x: Term, xs: Term) = _not_in:@(x, xs)
  def `{}`(x: Term) = _set_singleton:@(x)
  def set_disj(s: Term, t: Term) = _set_disj:@(s, t)
  def set_union(s: Term, t: Term) = _set_union:@(s, t)
  def nil = _nil
  def cons(x: Term, xs: Term) = _cons:@(x, xs)
  def elem(x: Term, xs: Term) = _elem:@(x, xs)
  def elems(xs: Term) = _elems:@(xs)
  def ++(x: Term, y: Term) = `_++`:@(x, y)
  
  class Brackets(left: String, right: String) extends Formula.Notation {
    import Formula._
    import report.data.TapeString._
    def format(term: Term) = {
      tape"${left}${display(term.subtrees.head)}${right}"
    }

    val precedence = 0
    override val arity = 1
  }
  
  import Formula.{M,O}
  Formula.INFIX ++= M(O("≠", 5), O("‖", 5), O("∈", 5), O("∉", 5), O("∪", 5), O("++", 5), O("⇒", 5)) + ("{.}" -> new Brackets("{", "}"))
}


trait Rules extends LazyLogging
{
  implicit val enc: Encoding
  
  val vars: List[Term]
  val rulesSrc: List[Term]
  
  lazy val rules = Rewrite.compileRules(vars, rulesSrc)
}

class BasicRules(implicit val enc: Encoding) extends Rules
{
  import BasicSignature._
  import Rewrite.RuleOps
  
  val vars = List(x, y, z, `x'`, xs, `xs'`)

  val rulesSrc = List(
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
      ~(in(x,y)) =:= not_in(x, y),
      not_in(x, xs) =:= set_disj(`{}`(x), xs),
      set_disj(xs, `{}`(x)) =:> not_in(x, xs),
      ~(x | y) =:= (~x & ~y),
      ~(x & y) =:= (~x | ~y),
      (x & (y & z)) =:= (x & y & z),
      (set_disj(x, xs) & set_disj(y, xs)) =:= (set_disj(set_union(x, y), xs)),
      (set_disj(xs, x) & set_disj(xs, y)) =:= (set_disj(xs, set_union(x, y))),
      elem(x, xs) =:= in(x, elems(xs)),
      elems(cons(`x'`, `xs'`)) =:= (set_union(`{}`(`x'`), elems(`xs'`))),  // <-- this one is somewhat superfluous?
      ++(cons(x, xs), `xs'`) =:= cons(x, ++(xs, `xs'`))      
  )
}

class AssocRules(implicit val enc: Encoding) extends Rules
{
  import BasicSignature._
  
  val vars = List(x, y, z, `x'`, xs, `xs'`)
  
  val rulesSrc = List(
      (x & (y & z)) =:= (x & y & z)
  )
}
