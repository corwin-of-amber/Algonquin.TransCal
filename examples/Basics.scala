package examples

import syntax.AstSugar._
import syntax.Formula
import relentless.rewriting.Rewrite
import relentless.matching.Encoding



object BasicSignature {
  val x = TV("x"); val y = TV("y"); val z = TV("z"); val w = TV("w"); val v = TV("v")
  val `x'` = TV("x'")
  val xs = TV("xs"); val `xs'` = TV("xs'")
  
  val _nil = TV("[]")
  val _cons = TV(":")
  val _elem = TV("elem")
  val _elems = TI("elems")

  val _in = TI("∈")
  val _not_in = TI("∉")
  val _set_singleton = TI("{.}")
  val _set_disj = TI("‖")
  val _set_union = TI("∪")
  
  def in(x: Term, xs: Term) = _in:@(x, xs)
  def not_in(x: Term, xs: Term) = _not_in:@(x, xs)
  def `{}`(x: Term) = _set_singleton:@(x)
  def set_disj(s: Term, t: Term) = _set_disj:@(s, t)
  def set_union(s: Term, t: Term) = _set_union:@(s, t)
  def cons(x: Term, xs: Term) = _cons:@(x, xs)
  def elem(x: Term, xs: Term) = _elem:@(x, xs)
  def elems(xs: Term) = _elems:@(xs)
  
  class Brackets(left: String, right: String) extends Formula.Notation {
    import Formula._
    import report.data.TapeString._
    def format(term: Term) = {
      tape"${left}${display(term.subtrees.head)}${right}"
    }

    val precedence: Int = 0
  }
  
  import Formula.{M,O}
  Formula.INFIX ++= M(O("‖", 1), O("∈", 1), O("∉", 1), O("∪", 1)) + ("{.}" -> new Brackets("{", "}"))
}


trait Rules
{
  implicit val enc: Encoding
  
  val vars: List[Term]
  val rulesSrc: List[Term]
  
  lazy val rules = Rewrite.compileRules(vars, rulesSrc)
}

class BasicRules(implicit val enc: Encoding) extends Rules
{
  import BasicSignature._
  
  val vars = List(x, y, z, `x'`, xs, `xs'`)
  
  val rulesSrc = List(
      (x =:= `x'`) =:= (in(`x'`, `{}`(x))),
      elem(x, cons(`x'`, `xs'`)) =:= ((x =:= `x'`) | elem(x, `xs'`)),
      ~(in(x,y)) =:= not_in(x, y),
      not_in(x, xs) =:= set_disj(`{}`(x), xs),
      ~(x | y) =:= (~x & ~y),
      (x & (y & z)) =:= (x & y & z),
      (set_disj(x, xs) & set_disj(y, xs)) =:= (set_disj(set_union(x, y), xs)),
      elem(x, xs) =:= in(x, elems(xs))
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
