package examples

import syntax._
import syntax.AstSugar._
import matching.Trie
import matching.Bundle
import matching.Match
import matching.Encoding
import matching.CompiledRule



object NotDup {
  
  import semantics.Prelude._
  
  val _nil = TV("[]")
  val _cons = TV(":")
  val _elem = TV("elem")
  val _x = TV("x")
  val _xs = TV("xs")
  
  val _notDup = TV("notDup")
  
  val notDup = (_nil ↦ TRUE) /: ((_cons:@(_x, _xs)) ↦ (~(_elem:@(_x, _xs)) & _notDup:@_xs))
 
  val `_x'` = TV("x'")
  val `_xs'` = TV("xs'")


  lazy implicit val enc = new matching.Encoding
  lazy val trie = new Trie[Int](new Tree(-1, List(new Tree(0, List(new Tree(1), new Tree(2), new Tree(3))))))

  def main(args: Array[String]) {
    
    println(notDup toPretty)
    
    
    val notDupEnc = enc.toTuples(notDup)
    
    //for (w <- notDupEnc) println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
    
    new Work(notDupEnc)()
  }
  
  
  class Work(init: List[Array[Int]]) {
    
    import collection.mutable
    
    val match_ = new Match(trie)(enc)
    
    val wq = mutable.Queue.empty[Array[Int]] ++ init
    val ws = mutable.Set.empty[List[Int]]
    
    def apply() {
      while (!wq.isEmpty) {
        val w = wq.dequeue()
        if (! (ws contains (w toList))) {
          ws += (w toList)
          work(w)
        }
      }
    }
     
    val xs_# = enc.ntor --> _xs.root
    val cons_# = enc.ntor --> _cons.root
    val elem_# = enc.ntor --> _elem.root
    val eq_# = enc.ntor --> I("=")
    
    val `xs = x':xs'` = new CompiledRule(
        List(new Bundle(List(Array(xs_#, ~0)))),
        new Scheme.Template(List(), _cons:@(`_x'`, `_xs'`)),
        1, List())
    
    val not_in = TI("∉")
    val set_singleton = TI("{.}")
    
    val `x=x' = x'∉{x}` = new CompiledRule(
        List(new Bundle(List(Array(eq_#, ~0, ~1, ~2)))),
        new Scheme.Template(List(_x, `_x'`) map (_.leaf), not_in:@(`_x'`, set_singleton:@_x)),
        3, List(1, 2))
    
    val `elem x (x':xs') = x=x' / elem x xs'` = new CompiledRule(
      List(new Bundle(List(Array(cons_#, ~2, ~3, ~4), Array(elem_#, ~0, ~1, ~2)))),
      new Scheme.Template(List(_x, `_x'`, `_xs'`) map (_.leaf), 
                          (_x =:= `_x'`) | (_elem:@(_x, `_xs'`))),
      5, List(1, 3, 4))
    
    def work(w: Array[Int]) {
      println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
      
      trie insert w
      
      processRule(`xs = x':xs'`, w)
      processRule(`x=x' = x'∉{x}`, w)
      processRule(`elem x (x':xs') = x=x' / elem x xs'`, w)
    }
    
    def processRule(rule: CompiledRule, w: Array[Int]) {
      val valuation = new Array[Int](rule.nHoles)
      for (s <- rule.shards) {
        for (valuation <- match_.matchLookupUnify_*(s.tuples, w, valuation)) {
          println(s"valuation = ${valuation mkString " "}")
          val add = enc.toBundle(rule.conclude(valuation)) fillIn valuation(0)
          wq.enqueue (add.toSeq:_*)
        }
      }
    }
    
  }
  
}