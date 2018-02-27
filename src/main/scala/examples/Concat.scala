package examples

import syntax.AstSugar._
import syntax.Piping._
import relentless.rewriting.Rewrite
import scala.collection.mutable.ListBuffer
import relentless.rewriting.Revision



object Concat {
  
  import BasicSignature._
  
  val _concat = TV("concat")
  val `_concat'` = TV("concat'")
  
  val concatProg = (nil ↦ nil) /: (cons(xs, xss) ↦ ++(xs, _concat:@xss))
  
  val _map = TV("map")
  val _concatMap = TV("concatMap")
  val `_concatMap'` = TV("concatMap'")
  
  val concatMapProg = f ↦: l ↦: (_concat:@(_map:@(f, l)))
  
  import NoDup.{locate, generalize, elaborate, enc, directory, State}
  
  import relentless.rewriting.RuleBasedTactic.{⇢, mkLocator, mkGoal}
  
  def main(args: Array[String]) {
    import BasicSignature._ 
    val BasicRules = new BasicRules
    val AssocRules = new AssocRules
    
    println(concatProg toPretty)
    
    val state0 = Revision(concatProg)
    val state1 = locate(state0, BasicRules.rules, mkLocator(x, y)(++(x,y), TI(1)))
    
    val state2 = generalize(state1, List.empty, List(xs, xss), Some(`_concat'`), List(xs, xss))
    
    val state3 = elaborate(state2 + (xs ⇢ cons(`x'`, `xs'`)), BasicRules.rules ++ ConcatRules1.rules, mkGoal(x, y)(cons(x, y), Some(TI(1))))
    
    val state4 = elaborate(state3, BasicRules.rules ++ ConcatRules1.rules ++ ConcatRules2.rules,
        mkGoal(x, y)(cons(`x'`, `_concat'`:@(x, y)), Some(TI(1))))
        
    
  }
  
  object ConcatRules1 extends Rules {
    import Rewrite.RuleOps
    val enc = NoDup.enc

    val vars = List(x, xs)
    val rulesSrc = List(
        (_concat:@nil) =:> nil,
        (_concat:@(cons(x, xs))) =:= ++(x, _concat:@xs)
      )
  }

  object ConcatRules2 extends Rules {
    import Rewrite.RuleOps
    val enc = NoDup.enc

    val vars = List(x, xs)
    val rulesSrc = List(
        (`_concat'`:@(x, xs)) =:= ++(x, _concat:@xs)
      )
  }
  
  object ConcatMapRules1 extends Rules {
    val enc = NoDup.enc
    
    val vars = List(l, f)
    val rulesSrc = List(
        (_concatMap:@(f,l)) =:= (_concat:@(_map:@(f, l)))
      )
  }
  
  /**
   * Profiles some (manually written) versions of 'concat' with a list of lists.
   */
  object Bench {
    import System.nanoTime
    def profile[R](code: => R, t: Long = nanoTime) = (code, nanoTime - t)
  
    def main(args: Array[String]) {
      val xss = (1 to 1000) map (i => (i * 100) to ((i+1) * 100) toList) toList
      
      val rep = 1
      
      profile { (0 until rep) foreach { i => xss flatten } } |> { case(_, tm) => println(s"${tm / 1000000}ms") }
      //profile { (0 until rep) foreach { i => concat(xss) } } |> { case(_, tm) => println(s"${tm / 1000000}ms") }
      profile { (0 until rep) foreach { i => (xss :\ ListBuffer.empty[Int])(_ ++: _) /*(xss reduce (_ ++ _))*/ } } |> { case(_, tm) => println(s"${tm / 1000000}ms") }
      profile { (0 until rep) foreach { i => flatten(xss) } } |> { case(_, tm) => println(s"${tm / 1000000}ms") }
    }
    
    def concat[A](l: List[List[A]]): List[A] = l match { case Nil => Nil case xs :: xss => xs ++ concat(xss)}
    
    def flatten[A](l: List[List[A]]): List[A] = {
      val b = new ListBuffer[A]
      for (x <- l) b ++= x;
      b.toList
    }
    //  l match { case Nil => Nil case xs :: xss => xs ++ concat(xss)}
  }

}