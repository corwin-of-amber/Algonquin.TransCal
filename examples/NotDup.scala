package examples

import syntax._
import syntax.AstSugar._
import matching.Trie
import matching.Bundle
import matching.Match
import matching.Encoding
import matching.CompiledRule
import matching.Rewrite
import java.io.FileOutputStream
import java.io.FileWriter



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
  lazy val trie = {
    def D(root: Trie.DirectoryEntry, subtrees: Tree[Trie.DirectoryEntry]*) = new Tree(root, subtrees.toList)
    new Trie[Int](D(-1, D(0, D(1, D(2), D(3)), D(2, D(3, D(4))), D(3))))
  }
  
  def main(args: Array[String]) {
    
    println(notDup toPretty)
    
    
    val notDupEnc = enc.toTuples(notDup)
    
    // Apply rewrite rules until saturated    
    val work = new Work(notDupEnc)
    
    work()

    println("-" * 60)
    
    // Show all representations of the program term (for debugging; this is slow)
    /*
    for (t <- new Reconstruct(enc.ntor --> notDup)())
      println(t map (enc.ntor <--) map (_.asInstanceOf[Identifier]) toPretty)

    println("-" * 60)
    */

    // Show terms matching the goal (_ & _ & _ & _)
    /*
    for (gm <- work.goalMatches;
         t <- new Reconstruct(gm)())
      println(t map (enc.ntor <--) map (_.asInstanceOf[Identifier]) toPretty)
    println("-" * 60)
    */

    for (gm <- work.goalMatches) {
      println(gm mkString " ")
      for (ln <- transposeAll(gm.toList drop 2 map (x => new Reconstruct(x)().toList), new Tree(0)))
        println("    " + mkStringColumns(ln map (t => if (t.root == 0) "" else t map (enc.ntor <--) map (_.asInstanceOf[Identifier]) toPretty), 40 ))
    }
    
    
    // Dump some things to files
    val encf = new FileWriter("enc")
    val pairs = enc.ntor.mapped.toList map { case (x,y) => (y,x) } sortBy (_._1);
    for ((k, v) <- pairs) { encf.write(s"${k} ${v}\n"); }
    encf.close()
    
    val tupf = new FileWriter("tuples")
    val words = trie.words sortBy (_(1))
    for (w <- words) { tupf.write(s"${w mkString " "}  [${w map (enc.ntor <--) mkString "] ["}]\n"); }
    tupf.close()
  }
  
  import collection.mutable.ListBuffer
  
  def transposeAll[A](xss: List[List[A]], ph: A): List[List[A]] = {
    val buf = new ListBuffer[List[A]]
    var yss = xss
    while (!yss.forall(_.isEmpty)) {
      buf += (yss map (_.headOption getOrElse ph))
      yss = (yss map { case _::tail => tail case _ => Nil })
    }
    buf.toList
  }
  
  def mkStringColumns[A](l: List[A], colWidth: Int) =
    l map (_.toString) map (s => s ++ (" " * (colWidth - s.length))) mkString " "


  object Rules
  {
    val x = TV("x"); val y = TV("y"); val z = TV("z")
    val `x'` = TV("x'")
    val xs = TV("xs"); val `xs'` = TV("xs'")
    
    val _in = TI("∈")
    val _not_in = TI("∉")
    val _set_singleton = TI("{.}")
    val _set_disj = TI("‖")
    val _set_union = TI("∪")
    
    val _elems = TI("elems")
    
    def in(x: Term, xs: Term) = _in:@(x, xs)
    def not_in(x: Term, xs: Term) = _not_in:@(x, xs)
    def `{}`(x: Term) = _set_singleton:@(x)
    def set_disj(s: Term, t: Term) = _set_disj:@(s, t)
    def set_union(s: Term, t: Term) = _set_union:@(s, t)
    def cons(x: Term, xs: Term) = _cons:@(x, xs)
    def elem(x: Term, xs: Term) = _elem:@(x, xs)
    def elems(xs: Term) = _elems:@(xs)
    
    import Rewrite.RuleOps
    
    val vars = List(x, y, z, `x'`, xs, `xs'`)
    
    val rulesSrc = List(
        (x =:= `x'`) =:= (in(`x'`, `{}`(x))),
        elem(x, cons(`x'`, `xs'`)) =:= ((x =:= `x'`) | elem(x, `xs'`)),
        ~(in(x,y)) =:= not_in(x, y),
        not_in(x, xs) =:= set_disj(`{}`(x), xs),
        ~(x | y) =:= (~x & ~y),
        (x & (y & z)) =:= (x & y & z),
        (set_disj(x, xs) & set_disj(y, xs)) =:= (set_disj(set_union(x, y), xs)),
        elem(x, xs) =:= in(x, elems(xs)),
        (_notDup:@(cons(x, xs))) =:= (~elem(x, xs) & (_notDup:@(xs)))
        )
        
    val rules = Rewrite.compileRules(vars, rulesSrc)
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
     
    /*
    val xs_# = enc.ntor --> _xs.root
    val cons_# = enc.ntor --> _cons.root
    val elem_# = enc.ntor --> _elem.root
    val eq_# = enc.ntor --> I("=")
    val notDup_# = enc.ntor --> _notDup.root
    */

    val `xs = x':xs'` = {
      import Rules._
      Rewrite.compileRules(List.empty, List(_xs =:= cons(`_x'`, `_xs'`)))
    }
    /*  new CompiledRule(
        new Scheme.Template()(_xs),
        new Scheme.Template()(_cons:@(`_x'`, `_xs'`)))
    */
    val in = TI("∈")
    val not_in = TI("∉")
    val set_singleton = TI("{.}")
    val set_disj = TI("‖")
    
    val _elems = TI("elems")
    
    /*
    val in_# = enc.ntor --> in.root
    val not_in_# = enc.ntor --> not_in.root
    val `∨_#` = enc.ntor --> ∨
    val `∧_#` = enc.ntor --> ∧
    val `¬_#` = enc.ntor --> ¬
    */
    
    
    val _y = TV("y")
    val _z = TV("z")
    val _w = TV("w")
    
    /*
    val `x=x' = x'∈{x}` = new CompiledRule(
        new Scheme.Template(_x, `_x'`)(_x =:= `_x'`),
        new Scheme.Template(_x, `_x'`)(in:@(`_x'`, set_singleton:@_x)))
    
    val `elem x (x':xs') = x=x' / elem x xs'` = new CompiledRule(
        new Scheme.Template(_x, `_x'`, `_xs'`)(_elem:@(_x, _cons:@(`_x'`, `_xs'`))),
        new Scheme.Template(_x, `_x'`, `_xs'`)
                          ((_x =:= `_x'`) | (_elem:@(_x, `_xs'`))))
    
    val `¬x∈y = x∉y` = new CompiledRule(
        new Scheme.Template(_x, _y)(~(in:@(_x, _y))),
        new Scheme.Template(_x, _y)(not_in:@(_x, _y)))
    
    val `x∉y = ¬x∈y` = new CompiledRule(
        new Scheme.Template(_x, _y)(not_in:@(_x, _y)),
        new Scheme.Template(List(_x, _y) map (_.leaf), ~(in:@(_x, _y))))
    
    val `x∉y = {x}‖y` = new CompiledRule(
        new Scheme.Template(_x, _y)(not_in:@(_x, _y)),
        new Scheme.Template(_x, _y)(set_disj:@(set_singleton(_x), _y)))
    
    val `¬(x ∨ y) = ¬x ∧ ¬y` = new CompiledRule(
        new Scheme.Template(_x, _y)(~(_x | _y)),
        new Scheme.Template(_x, _y)(~_x & ~_y))
    
    val `x ∧ (y ∧ z) = (x ∧ y) ∧ z` = new CompiledRule(
        new Scheme.Template(_x, _y, _z)(_x & (_y & _z)),
        new Scheme.Template(_x, _y, _z)(_x & _y & _z))

    val `elem x y = x ∈ elems y` = new CompiledRule(
        new Scheme.Template(_x, _xs)(_elem:@(_x, _xs)),
        new Scheme.Template(_x, _xs)(in:@(_x, _elems:@(_xs))))
    
    val `notDup (x:xs) = ¬(elem x xs) ∧ notDup xs` = new CompiledRule(
        new Scheme.Template(_x, _xs)(_notDup:@(_cons:@(_x, _xs))),
        new Scheme.Template(_x, _xs)(~(_elem:@(_x, _xs)) & _notDup:@(_xs)))
    */
    val _goalMarker = $TI("gem")
    
    import Rewrite.RuleOps
    
    val goal = Rewrite.compileRules(List(_x, _y, _z, _w), List((_x & _y & _z) =:> _goalMarker(_x, _y, _z))) /*new CompiledRule(
        new Scheme.Template(_x, _y, _z /*, _w*/)(_x & _y & _z /*& _w*/),
        new Scheme.Template(_x, _y, _z /*, _w*/)(_goalMarker(_x, _y, _z/*, _w*/)))*/
    
    def work(w: Array[Int]) {
      println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
      
      trie add w
      
      for (r <- `xs = x':xs'` ::: Rules.rules) processRule(r, w)
      /*
      processRule(`xs = x':xs'`, w)
      processRule(`x=x' = x'∈{x}`, w)
      processRule(`elem x (x':xs') = x=x' / elem x xs'`, w)
      processRule(`¬x∈y = x∉y`, w)
      processRule(`x∉y = ¬x∈y`, w)
      processRule(`x∉y = {x}‖y`, w)
      processRule(`¬(x ∨ y) = ¬x ∧ ¬y`, w)
      processRule(`x ∧ (y ∧ z) = (x ∧ y) ∧ z`, w)
      processRule(`elem x y = x ∈ elems y`, w)
      processRule(`notDup (x:xs) = ¬(elem x xs) ∧ notDup xs`, w)
      */
      
      for (g <- goal) processRule(g, w)
    }
    
    def processRule(rule: CompiledRule, w: Array[Int]) {
      val valuation = new Array[Int](rule.nHoles)
      for (s <- rule.shards) {
        for (valuation <- match_.matchLookupUnify_*(s.tuples, w, valuation)) {
          println(s"valuation = ${valuation mkString " "}")
          wq.enqueue (rule.conclude(valuation, trie):_*)
          //val add = enc.toBundle()(rule.conclude(valuation)) fillIn valuation(0)
          //wq.enqueue (add.toSeq:_*)
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