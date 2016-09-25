package examples

import syntax._
import syntax.AstSugar._
import relentless.matching.Trie
import relentless.matching.Bundle
import relentless.matching.Match
import relentless.matching.Encoding
import relentless.rewriting.CompiledRule
import relentless.rewriting.Rewrite
import java.io.FileOutputStream
import java.io.FileWriter



object NoDup {
  
  import semantics.Prelude._
  
  val _nil = TV("[]")
  val _cons = TV(":")
  val _elem = TV("elem")
  val _x = TV("x")
  val _xs = TV("xs")
  
  val _nodup = TV("nodup")
  val `_nodup'` = TV("nodup'")
  
  val nodup = (_nil ↦ TRUE) /: ((_cons:@(_x, _xs)) ↦ (~(_elem:@(_x, _xs)) & _nodup:@_xs))
 
  val `_x'` = TV("x'")
  val `_xs'` = TV("xs'")


  lazy implicit val enc = new Encoding
  lazy val directory = {
    def D(root: Trie.DirectoryEntry, subtrees: Tree[Trie.DirectoryEntry]*) = new Tree(root, subtrees.toList)
    D(-1, D(0, D(1, D(2, D(3, D(4))), D(3)), D(2, D(3, D(4))), D(3)))
  }


  val _phMarker = $TI("orb", "operator")
  val _goalMarker = $TI("gem", "operator")
  
  val start = {
    import Rewrite.RuleOps
    import BasicSignature._
    
    Rewrite.compileRules(List(_x, y), List((_x & y) =:> _phMarker(TI(1))))
  }
  
  val goal1 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(x, y, z, w, v),
        List((_phMarker(TI(1)) ||| (x & y & z & w)) =:> _goalMarker(x, y, z, w))
      )
  }
  
  val goal2 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(y, z, w, v),
        List((not_in(x, elems(`xs'`)) & not_in(`x'`, elems(`xs'`))) =:> _phMarker(TI(2)),
            (_phMarker(TI(2)) ||| (set_disj(y, z))) =:> _goalMarker(y, z))
      )
  }
  
  val goal3 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(x, y, z, w, v),
      List(((set_disj(set_union(y, z), w)) & v) =:> _phMarker(TI(3)),
           (_phMarker(TI(3)) ||| ((`_nodup'`:@(y, z)))) =:> _goalMarker(y, z))
      )
  }
  
  case class State(val program: Term, val elaborate: List[(Term, Term)], val tuples: List[Array[Int]])(implicit val enc: Encoding) {
    def this(program: Term)(implicit enc: Encoding) = this(program, List.empty, enc.toTuples(program))
    
    def +(el: (Term, Term)) = State(program, elaborate :+ el, tuples ++ enc.toTuples(el._2, el._1))
    def ++(l: Iterable[Array[Int]]) = State(program, elaborate, tuples ++ l)
  }
  object State {
    def apply(program: Term)(implicit enc: Encoding) = new State(program)
  }
  
  // --------------------------
  // low-level state primitives
  // --------------------------
  
  def locate(s: State, rules: List[CompiledRule]) = {
    // Apply rules to find the pattern
    val work0 = new Work(s.tuples, rules, directory)
    work0()
    println("-" * 60)

    for (gm <- work0.matches(_phMarker.leaf);
         t <- new Reconstruct(gm(1), work0.nonMatches(_phMarker.leaf, _goalMarker.leaf))(enc))
      println("    " + (t toPretty))
    
    // Get the heads of all the terms matched by the pattern
    s ++ work0.matches(_phMarker.leaf)
  }
  
  def generalize(s: State, rules: List[CompiledRule], leaves: List[Term], context: List[Term]): State = {
    // Apply rewrite rules until saturated    
    val work = new Work(s.tuples, rules, directory)
    work()
    println("-" * 60)
    
    // Reconstruct and generalize
    for (gm <- work.matches(_phMarker.leaf);
         t <- new Reconstruct(gm(1), work.nonMatches(_phMarker.leaf, _goalMarker.leaf))(enc);
         tg <- { import BasicSignature._ ; generalize(t, leaves, context) }) {
      println("    " + t.toPretty)
      println("    as  " + tg.toPretty)
    }
    
    s // TODO
  }
  
  def elaborate(s: State, rules: List[CompiledRule]) = {
    val work = new Work(s.tuples, rules, directory)
    work()
    println("-" * 60)
    
    showem(work.goalMatches, work.trie)
    
    s // TODO
  }
  
  def main(args: Array[String]) {
    import BasicSignature._ 
    val BasicRules = new BasicRules
    val AssocRules = new AssocRules
    
    println(nodup toPretty)
    
    val state0 = State(nodup)
    
    // Find matches for source pattern (_ ∧ _) - these are marked as 1⃝
    val state0_ = locate(state0, AssocRules.rules ++ start)

    // Generalize  [ {x}, xs ]
    val state1 = generalize(state0_, BasicRules.rules, List(`{}`(x), xs), List(x, xs))
    
    // 1⃝  ⇢  x' ∉ {x}  ∧  x ∉ elems xs'  ∧  x' ∉ elems xs'  ∧  nodup xs'
    val state2 = elaborate(state1, BasicRules.rules ++ NoDupRules1.rules /*++ NoDupRules2.rules*/ ++ goal1)
    
    // x ∉ elems xs'  ∧  x' ∉ elems xs'  ⇢  ({x} ∪ {x'}) ‖ elems xs'
    val state3 = elaborate(state2, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules ++ goal2)
    
    // 1⃝  ⇢  x' ∉ {x}  ∧  nodup' ({x} ∪ {x'}) xs'
    val state4 = elaborate(state3, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules ++ goal3)


    dump(state4)
  }
  
  def dump(state: State) {
    
    // Dump some things to files
    val encf = new FileWriter("enc")
    val pairs = state.enc.ntor.mapped.toList map { case (x,y) => (y,x) } sortBy (_._1);
    for ((k, v) <- pairs) { encf.write(s"${k} ${v}\n"); }
    encf.close()
    
    val tupf = new FileWriter("tuples")
    val words = state.tuples sortBy (_(1))
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


  def showem(matches: Seq[Array[Int]], trie: Trie[Int]) {
    for (gm <- matches) {
      println(gm mkString " ")
      for (ln <- transposeAll(gm.toList drop 2 map (x => new Reconstruct(x, trie)(enc).toList), B))
        println("    " + mkStringColumns(ln map (t => if (t == B) "" else t toPretty), 40 ))
    }
  }
  
  def generalize(t: Term, leaves: List[Term], context: List[Term]): Option[Term] =
    leaves.indexOf(t) match {
      case -1 => if (context contains t) None else T_?(t.root)(t.subtrees map (generalize(_, leaves, context)))
      case idx => Some(TI(idx))
    }
  
  def T_?(root: Identifier)(subtrees: List[Option[Term]]) = 
    if (subtrees exists (_ == None)) None else Some(T(root)(subtrees map (_.get)))
  
  object NoDupRules1 extends Rules {
    import BasicSignature._
    val enc = NoDup.enc

    val ys = TI("ys")
    val vars = List(y, ys)
    val rulesSrc = List(
        xs =:= cons(`x'`, `xs'`),
        (_nodup:@(cons(y, ys))) =:= (~elem(y, ys) & (_nodup:@(ys)))
    )
  }
  
  object NoDupRules2 extends Rules {
    import BasicSignature._
    val enc = NoDup.enc

    val vars = List(x, xs)
    val rulesSrc = List(
        (`_nodup'`:@(x, xs)) =:= (set_disj(x, elems(xs)) & _nodup:@(xs))
    )
  }
        
  class Work(init: Seq[Array[Int]], compiledRules: List[CompiledRule], val trie: Trie[Int]) {
    
    import collection.mutable
    
    def this(init: Seq[Array[Int]], compiledRules: List[CompiledRule], directory: Tree[Trie.DirectoryEntry]) =
      this(init, compiledRules, new Trie[Int](directory))

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
    
    def work(w: Array[Int]) {
      //println((w mkString " ") + "   [" + (w map (enc.ntor <--) mkString "] [") + "]")
      
      trie add w
      
      for (r <- compiledRules) processRule(r, w)
      
      //for (g <- goal) processRule(g, w)
    }
    
    def processRule(rule: CompiledRule, w: Array[Int]) {
      val valuation = new Array[Int](rule.nHoles)
      for (s <- rule.shards) {
        for (valuation <- match_.matchLookupUnify_*(s.tuples, w, valuation)) {
          //println(s"valuation = ${valuation mkString " "}")
          val add = rule.conclude(valuation, trie)
          wq.enqueue (add:_*)
        }
      }
    }
    
    def matches(headSymbol: Identifier) = {
      trie.get(0, enc.ntor --> headSymbol) match {
        case Some(t) => t.words
        case _ => Seq.empty
      }
    }
    
    def nonMatches(headSymbols: Identifier*) = {
      val heads = headSymbols map (enc.ntor -->)
      trie.words filterNot (heads contains _(0))
    }
    
    def goalMatches = matches(_goalMarker.leaf)
    
  }
  

  /**
   * Reconstruction of terms from tuples stored in the trie.
   */
  class Reconstruct private (init: Tree[Int], trie: Trie[Int]) {
    
    import collection.mutable
    import math.Ordering
    import Reconstruct._

    def this(root: Int, trie: Trie[Int]) = this(new Tree(root), Reconstruct.mkTrie1(trie.words))
    def this(root: Int, words: Seq[Array[Int]]) = this(new Tree(root), Reconstruct.mkTrie1(words))
    def this(tuple: Array[Int], trie: Trie[Int]) = this(Reconstruct.tupleToTree(tuple), Reconstruct.mkTrie1(trie.words))
    
    case class Entry(val t: Tree[Int]) {
      val pri = -t.size
    }
        
    val pq = new mutable.PriorityQueue[Entry]()(Ordering.by(_.pri)) += Entry(init)
    val ws = mutable.Set.empty[Tree[Int]]

    def apply() = {
      whileYield (!pq.isEmpty, {
        val e = pq.dequeue()
        var expanded = false
        for (leaf <- e.t.leaves if !expanded;
             alt <- trie.get(1, leaf.leaf) map (_.words) getOrElse List()) {
          val expand = e.t.replaceDescendant((leaf, tupleToTree(alt)))
          expanded = true
          if (ws add expand)
            pq enqueue Entry(expand)
        }
        if (expanded) None else Some(e.t)
      }) flatten
    }
    
    def apply(enc: Encoding): Stream[Term] = {
      for (t <- apply()) yield decode(t)(enc)
    }
    
    def decode(t: Tree[Int])(implicit enc: Encoding): Term = {
      //println(enc.ntor <-- t.root)
      enc.ntor <-- t.root match {
        case r: Identifier =>
          r.kind match {
            case "operator" | "connective" | "quantifier" => T(r)(t.subtrees map decode)
            case _ => T(r):@(t.subtrees map decode)
          }
        case t: Tree[_] => t.asInstanceOf[Term] // hope there are no other trees
      }
    }
  }
  
  object Reconstruct {
    
    /** Build a trie indexed by location 1 */
    def mkTrie1(words: Seq[Array[Int]]) = {
      val t = new Trie[Int](new Tree(-1, List(new Tree(1))))
      words foreach t.add
      t
    }
    
    def tupleToTree(tuple: Array[Int]) = new Tree(tuple(0), tuple drop 2 map (new Tree(_)) toList)
    
    def onlyIf[A](cond: Boolean, op: => Seq[A]) = if (cond) op else Seq.empty
    def whileYield[A](cond: => Boolean, vals: => A) = takeWhileLeft(cond, Iterator.continually(vals))
    def takeWhileLeft[A](cond: => Boolean, it: Iterator[A]): Stream[A] =
      if (!cond) Stream.empty
      else it.next #:: takeWhileLeft(cond, it)
    
  }
    
  
}