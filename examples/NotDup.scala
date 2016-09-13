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
    
    // Apply rewrite rules until saturated    
    val work = new Work(notDupEnc)
    
    work()

    println("-" * 60)
    
    // Show all representations of the program term
    for (t <- new Reconstruct(enc.ntor --> notDup)())
      println(t map (enc.ntor <--) map (_.asInstanceOf[Identifier]) toPretty)

    println("-" * 60)

    // Show terms matching the goal (_ & _ & _ & _)
    for (gm <- work.goalMatches;
         t <- new Reconstruct(gm)())
      println(t map (enc.ntor <--) map (_.asInstanceOf[Identifier]) toPretty)
  }
  
  
  class Work(init: List[Array[Int]]) {
    
    import collection.mutable
    
    val match_ = new Match(trie)(enc)
    
    val wq = mutable.Queue.empty[Array[Int]] ++ init
    val ws = mutable.Set.empty[List[Int]]
    
    def apply() {
      while (!wq.isEmpty) {
        val w = wq.dequeue()
        if (ws add (w toList)) {
          work(w)
        }
      }
    }
     
    val xs_# = enc.ntor --> _xs.root
    val cons_# = enc.ntor --> _cons.root
    val elem_# = enc.ntor --> _elem.root
    val eq_# = enc.ntor --> I("=")
    
    val notDup_# = enc.ntor --> _notDup.root
    
    val `xs = x':xs'` = new CompiledRule(
        List(new Bundle(List(Array(xs_#, ~0)))),
        new Scheme.Template(List(), _cons:@(`_x'`, `_xs'`)),
        1, List())
    
    val in = TI("∈")
    val not_in = TI("∉")
    val set_singleton = TI("{.}")
    
    val in_# = enc.ntor --> in.root
    val `∨_#` = enc.ntor --> ∨
    val `∧_#` = enc.ntor --> ∧
    val `¬_#` = enc.ntor --> ¬
    
    val _y = TV("y")
    val _z = TV("z")
    
    val `x=x' = x'∈{x}` = new CompiledRule(
        List(new Bundle(List(Array(eq_#, ~0, ~1, ~2)))),
        new Scheme.Template(List(_x, `_x'`) map (_.leaf), in:@(`_x'`, set_singleton:@_x)),
        3, List(1, 2))
    
    val `elem x (x':xs') = x=x' / elem x xs'` = new CompiledRule(
      List(new Bundle(List(Array(cons_#, ~2, ~3, ~4), Array(elem_#, ~0, ~1, ~2)))),
      new Scheme.Template(List(_x, `_x'`, `_xs'`) map (_.leaf), 
                          (_x =:= `_x'`) | (_elem:@(_x, `_xs'`))),
      5, List(1, 3, 4))
    
    val `¬x∈y = x∉y` = new CompiledRule(
        List(new Bundle(List(Array(`¬_#`, ~0, ~1), Array(in_#, ~1, ~2, ~3))),
             new Bundle(List(Array(in_#, ~1, ~2, ~3), Array(`¬_#`, ~0, ~1)))),
        new Scheme.Template(List(_x, _y) map (_.leaf), not_in:@(_x, _y)),
        4, List(2, 3))
    
    val `¬(x ∨ y) = ¬x ∧ ¬y` = new CompiledRule(
        List(new Bundle(List(Array(`∨_#`, ~1, ~2, ~3), Array(`¬_#`, ~0, ~1)))),
        new Scheme.Template(List(_x, _y) map (_.leaf), ~_x & ~_y),
        4, List(2, 3))
    
    val `x ∧ (y ∧ z) = (x ∧ y) ∧ z` = new CompiledRule(
        List(new Bundle(List(Array(`∧_#`, ~0, ~1, ~2), Array(`∧_#`, ~2, ~3, ~4))),
             new Bundle(List(Array(`∧_#`, ~2, ~3, ~4), Array(`∧_#`, ~0, ~1, ~2)))),
        new Scheme.Template(List(_x, _y, _z) map (_.leaf), _x & _y & _z),
        5, List(1, 3, 4))

    val `notDup (x:xs) = ¬(elem x xs) ∧ notDup xs` = new CompiledRule(
        List(new Bundle(List(Array(cons_#, ~1, ~2, ~3), Array(notDup_#, ~0, ~1)))),
        new Scheme.Template(List(_x, _xs) map (_.leaf), ~(_elem:@(_x, _xs)) & _notDup:@(_xs)),
        4, List(2, 3))
    
    val _goalMarker = $TI("gem")
    
    val goal = new CompiledRule(
        List(new Bundle(List(Array(`∧_#`, ~0, ~1, ~2),
                             Array(`∧_#`, ~1, ~3, ~4),
                             Array(`∧_#`, ~3, ~5, ~6))),
             new Bundle(List(Array(`∧_#`, ~1, ~3, ~4),
                             Array(`∧_#`, ~0, ~1, ~2),
                             Array(`∧_#`, ~3, ~5, ~6))),
             new Bundle(List(Array(`∧_#`, ~3, ~5, ~6),
                             Array(`∧_#`, ~1, ~3, ~4),
                             Array(`∧_#`, ~0, ~1, ~2)))),
        new Scheme.Template(List(I(1), I(2), I(3), I(4)), _goalMarker(TI(1), TI(2), TI(3), TI(4))),
        7, List(5, 6, 4, 2))
    
    def work(w: Array[Int]) {
      println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
      
      trie add w
      
      processRule(`xs = x':xs'`, w)
      processRule(`x=x' = x'∈{x}`, w)
      processRule(`elem x (x':xs') = x=x' / elem x xs'`, w)
      processRule(`¬x∈y = x∉y`, w)
      processRule(`¬(x ∨ y) = ¬x ∧ ¬y`, w)
      processRule(`x ∧ (y ∧ z) = (x ∧ y) ∧ z`, w)
      processRule(`notDup (x:xs) = ¬(elem x xs) ∧ notDup xs`, w)
      
      processRule(goal, w)
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
    
    def goalMatches = {
      trie.get(0, enc.ntor --> _goalMarker.leaf) match {
        case Some(t) => t.words
        case _ => Seq.empty
      }
    }
    
  }
  

  /**
   * Reconstruction of terms from tuples stored in the trie.
   */
  class Reconstruct(init: Tree[Int]) {
    
    import collection.mutable
    import math.Ordering
    import Reconstruct._

    def this(root: Int) = this(new Tree(root))
    def this(tuple: Array[Int]) = this(Reconstruct.tupleToTree(tuple))
    
    case class Entry(val t: Tree[Int]) {
      val pri = -t.size
    }
    
    // copy from trie indexed by location 1
    val trie1 = {
      val t = new Trie[Int](new Tree(-1, List(new Tree(1))))
      trie.words foreach t.add
      t
    }
    
    val pq = new mutable.PriorityQueue[Entry]()(Ordering.by(_.pri)) += Entry(init)
    val ws = mutable.Set.empty[Tree[Int]]

    def apply() = {
      whileYield (!pq.isEmpty, {
        val e = pq.dequeue()
        var expanded = false
        for (leaf <- e.t.leaves if !expanded;
             alt <- trie1.get(1, leaf.leaf) map (_.words) getOrElse List()) {
          val expand = e.t.replaceDescendant((leaf, tupleToTree(alt)))
          expanded = true
          if (ws add expand)
            pq enqueue Entry(expand)
        }
        if (expanded) None else Some(e.t)
      }) flatten
    }
  }
  
  object Reconstruct {
    
    def tupleToTree(tuple: Array[Int]) = new Tree(tuple(0), tuple drop 2 map (new Tree(_)) toList)
    
    def onlyIf[A](cond: Boolean, op: => Seq[A]) = if (cond) op else Seq.empty
    def whileYield[A](cond: => Boolean, vals: => A) = takeWhileLeft(cond, Iterator.continually(vals))
    def takeWhileLeft[A](cond: => Boolean, it: Iterator[A]): Stream[A] =
      if (!cond) Stream.empty
      else it.next #:: takeWhileLeft(cond, it)
    
  }
    
  
}