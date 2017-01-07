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
import relentless.rewriting.Reconstruct
import relentless.rewriting.Rewrite.WorkLoop
import report.data.DisplayContainer



object NoDup {
  
  import semantics.Prelude._
  
  val _nil = TV("[]")
  val _cons = TV("::")
  val _elem = TV("elem")
  val _x = TV("x")
  val _xs = TV("xs")
  
  val _nodup = TV("nodup")
  val `_nodup'` = TV("nodup'")
  
  val nodupProg = (_nil ↦ TRUE) /: ((_cons:@(_x, _xs)) ↦ (~(_elem:@(_x, _xs)) & _nodup:@_xs))
 
  val `_x'` = TV("x'")
  val `_xs'` = TV("xs'")


  lazy implicit val enc = new Encoding
  lazy val directory = {
    def D(root: Trie.DirectoryEntry, subtrees: Tree[Trie.DirectoryEntry]*) = new Tree(root, subtrees.toList)
    D(-1, D(0, D(1, D(2, D(3, D(4))), D(3)), D(2, D(3, D(4))), D(3)))
  }


  def main(args: Array[String]) {
    import BasicSignature._ 
    val BasicRules = new BasicRules
    val AssocRules = new AssocRules
    
    println(nodupProg toPretty)
    
    val state0 = State(nodupProg)
    
    // Find matches for source pattern (_ ∧ _) - these are marked as 1⃝
    val state0_ = locate(state0, AssocRules.rules, mkLocator(x, y)(x & y, TI(1)))

    // Generalize   1⃝  ⇢  nodup' {x} xs
    val state1 = generalize(state0_, BasicRules.rules, List(`{}`(x), xs), Some(`_nodup'`), List(x, xs))
    
    // Let  xs  =  x' : xs'
    val state1_ = state1 +- (_xs, cons(`x'`, `xs'`))
    
    // 1⃝  ⇢  x' ∉ {x}  ∧  x ∉ elems xs'  ∧  x' ∉ elems xs'  ∧  nodup xs'
    val state2 = elaborate(state1_, BasicRules.rules ++ NoDupRules1.rules, mkGoal(x, y, z, w)(x & y & z & w, Some(TI(1))))
    
    // x ∉ elems xs'  ∧  x' ∉ elems xs'  ⇢  ({x} ∪ {x'}) ‖ elems xs'
    val state2_ = locate(state2, BasicRules.rules, mkLocator()(not_in(x, elems(`xs'`)) & not_in(`x'`, elems(`xs'`)), TI(2)))
    val state3 = elaborate(state2_, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules, mkGoal(y, z)(set_disj(y, z), Some(TI(2))))
    
    // 1⃝  ⇢  x' ∉ {x}  ∧  nodup' ({x} ∪ {x'}) xs'
    val state4 = elaborate(state3, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules, 
        mkLocator_simple(x, y, z, w)(set_disj(set_union(y, z), w) & x, TI(3)) ++
        mkGoal(x, y)(`_nodup'`:@(x, y), Some(TI(3))))

    // Note: this is equivalent but slower:
    //val state3_ = locate(state3, BasicRules.rules, mkLocator_simple(x, y, z, w)(set_disj(set_union(y, z), w) & x, TI(3)))
    //val state4 = elaborate(state3_, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules, 
    //    mkGoal(x, y)(`_nodup'`:@(x, y), Some(TI(3))))

    dump(state4)
    
    // Derive nodup' -- this part should be more automatic
    val a = TV("a")
    val `nodup' a x:xs` = `_nodup'`:@(a, cons(`x'`, `xs'`))
    
    val state10 = State( `nodup' a x:xs` )
    elaborate(state10, BasicRules.rules ++ NoDupRules1.rules ++ NoDupRules2.rules, mkGoal(x, y, z, w)(not_in(x,y) & (`_nodup'`:@(z, w))))
    /*
    val QuantifiedRules = new QuantifiedRules
    val state4_ = locate(state4, BasicRules.rules ++ anchor4, TI(4), None)
    // does not terminate lol :)
    explore(state4_, BasicRules.rules ++ QuantifiedRules.rules, TI(4))

    dump(state4)
    */
  }
  
  class QuantifiedRules(implicit val enc: Encoding) extends Rules {
    import BasicSignature._
    
    val vars = List(x, y, z, `x'`, xs, `xs'`)
    
    val rulesSrc = List(
        set_disj(x, y) =:= ∀(z)(~(in(z, x) & in (z, y)))
    )
  }

  import Rewrite._goalMarker
  val _phMarker = $TI("⨀", "marker")
  val _phmMarker = $TI("⨀⋯", "marker")
  
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
  val goal1scheme = { import BasicSignature._; new Scheme.Template(x, y, z, w)(x & y & z & w) }
  
  val anchor2 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(),
        List((not_in(x, elems(`xs'`)) & not_in(`x'`, elems(`xs'`))) =:> (_phMarker(TI(2)) ||| _phmMarker(TI(2), x, `x'`, `xs'`)))
      )
  }
  val anchor2scheme = { import BasicSignature._; new Scheme.Template(x, `x'`, `xs'`)(not_in(x, elems(`xs'`)) & not_in(`x'`, elems(`xs'`))) }
  
  val goal2 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(x, `x'`, `xs'`, y, z, w, v),
        List(//(not_in(x, elems(`xs'`)) & not_in(`x'`, elems(`xs'`))) =:> _phMarker(TI(2)),
            (_phMarker(TI(2)) ||| (set_disj(y, z))) =:> _goalMarker(y, z))
      )
  }
  val goal2scheme = { import BasicSignature._; new Scheme.Template(y, z)(set_disj(y, z)) }
  
  val goal3 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(x, y, z, w, v),
      List(((set_disj(set_union(y, z), w)) & v) =:> _phMarker(TI(3)),
           (_phMarker(TI(3)) ||| ((`_nodup'`:@(y, z)))) =:> _goalMarker(y, z))
      )
  }
  val goal3scheme = { import BasicSignature._; new Scheme.Template(y, z)(`_nodup'`:@(y, z)) }
  
  val anchor4 = {
    import Rewrite.RuleOps
    import BasicSignature._

    Rewrite.compileRules(List(x,y),
        List(set_disj(x, y) =:> _phMarker(TI(4))))
  }
  //val goal4scheme = { import BasicSignature._; new Scheme.Template(y)(y) }
  
  class CompiledGoal(val rules: List[CompiledRule], val scheme: Scheme)
  
  class CompiledPattern(val rules: List[CompiledRule], val scheme: Option[Scheme], val anchor: Term) {
    def ++(goal: CompiledGoal) = new CompiledGoal(rules ++ goal.rules, goal.scheme)  // note: omits this.scheme
  }
  
  def mkGoal(vars: Term*)(pattern: Term, anchor: Option[Term] = None) = {
    import Rewrite.RuleOps
    val rules = Rewrite.compileRules(vars toList, List(anchor match {
      case None => pattern =:> _goalMarker(vars toList)
      case Some(anchor) => (_phMarker(anchor) ||| pattern) =:> _goalMarker(vars toList)
    }))
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledGoal(rules, tmpl)
  }
  
  def mkLocator(vars: Term*)(pattern: Term, anchor: Term) = {
    import Rewrite.RuleOps
    // _phMarker(anchor) is so that it fits the goal pattern
    // _phmMarker(anchor, vars...) is for locate() to be able to reconstruct the term using tmpl
    val rules = Rewrite.compileRules(vars toList, List(pattern =:> (_phMarker(anchor) ||| _phmMarker(anchor :: vars.toList))))
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledPattern(rules, Some(tmpl), anchor)
  }
  
  def mkLocator_simple(vars: Term*)(pattern: Term, anchor: Term) = {
    import Rewrite.RuleOps
    // _phMarker(anchor) is so that it fits the goal pattern
    val rules = Rewrite.compileRules(vars toList, List(pattern =:> _phMarker(anchor)))
    new CompiledPattern(rules, None, anchor)
  }
  
  case class State(val program: Term, val focusedSubterm: Map[Int, Term], val elaborate: List[(Term, Term)], val tuples: List[Array[Int]])(implicit val enc: Encoding) {
    def this(program: Term)(implicit enc: Encoding) = this(program, Map.empty, List.empty, enc.toTuples(program))
    
    lazy val trie = new Trie[Int](directory) ++= tuples
    
    def at(subterms: Map[Int, Term]) = State(program, subterms, elaborate, tuples)
    
    def indexMapping: Map[Int, Term] = program.nodes ++ (elaborate flatMap (_._2.nodes)) map (t => (enc.ntor --> t, t)) toMap
    def incorporate(term: Term, as: Term) = enc.toTuples(term, as)

    def +(el: (Term, Term)) = State(program, focusedSubterm, elaborate :+ el, tuples ++ incorporate(el._2, el._1))
    def ++(els: Iterable[(Term, Term)]) = State(program, focusedSubterm, elaborate ++ els, tuples ++ (els flatMap (el => incorporate(el._2, el._1))))
    def ++(l: Iterable[Array[Int]])(implicit d: DummyImplicit) = State(program, focusedSubterm, elaborate, tuples ++ l)
    
    def +-(el: (Term, Term)) = State(program, focusedSubterm, elaborate :+ el, tuples)  // add but not incorporate: this is a bit weird, but makes sense if there is an appropriate rule in place
  }
  object State {
    def apply(program: Term)(implicit enc: Encoding) = new State(program)
  }
  
  def compaction(work: WorkLoop): WorkLoop = {
    val trie = work.trie
    val equiv = collection.mutable.Map.empty[Int, Int]
    val except = List(_goalMarker, _phMarker, _phmMarker) map (enc.ntor --> _.leaf)
    /**
     * uniques() group words in given trie by values at locations >= index,
     * then declares _(1) to be equivalent for all words in each group.
     */
    def uniques(trie: Trie[Int], index: Int) {
      if (index >= trie.subtries.length || trie.subtries(index) == null) {
        if (trie.words.length > 1) {
          val equals = trie.words map (_(1))
          //println(equals mkString " = ")
          val rep = equals.min
          equals foreach (equiv += _ -> rep)
        }
      }
      else {
        for ((k, subtrie) <- trie.subtries(index)) uniques(subtrie, index+1)
      }
    }
    for ((k, subtrie) <- trie.subtries(0) if !(except contains k)) {
      uniques(subtrie, 2)
    }
    if (equiv.nonEmpty) {
      def subst(w: Array[Int]) = w map (x => equiv getOrElse (x,x))
      val work = new WorkLoop(trie.words.toStream map subst, List.empty, trie.directory)
      work()
      compaction(work)
    }
    else work
  }
  
  def spanning(trie: Trie[Int], init: Iterable[Int], boundary: Iterable[Int]) = {
    import collection.mutable
    val ws = mutable.Set.empty ++ boundary
    val wq = mutable.Queue.empty ++ init
    Reconstruct.whileYield(wq.nonEmpty) {
      val u = wq.dequeue
      if (ws add u) {
        val incident = 1 until trie.subtries.length flatMap (i => trie.get(i, u).toList flatMap (_.words))
        for (e <- incident; v <- e drop 1 if !(ws contains v)) wq enqueue v
        incident
      }
      else Seq.empty
    } flatten
  }
  
  // --------------------------
  // low-level state primitives
  // --------------------------
  
  def locate(s: State, rules: List[CompiledRule], pattern: CompiledPattern): State = {
    locate(s, rules ++ pattern.rules, pattern.anchor, pattern.scheme)
  }

  def locate(s: State, rules: List[CompiledRule], anchor: Term, anchorScheme: Option[Scheme]) = {
    // Apply rules to find the pattern
    val work0 = new WorkLoop(s.tuples, rules, directory)
    work0()
    println("-" * 60)

    val anchor_# = enc.ntor --> anchor
    val marks = work0.matches(_phMarker.leaf) filter (_(2) == anchor_#)
    val matches = work0.matches(_phmMarker.leaf) filter (_(2) == anchor_#)
    val nonMatches = work0.nonMatches(_phMarker.leaf, _phmMarker.leaf, _goalMarker.leaf)

    // choose the first term for each match
    val im = s.indexMapping
    val subterms = anchorScheme match {
      case Some(s) =>
        matches map { gm =>
          val components = (new Reconstruct(gm, nonMatches) ++ im)(enc).head.subtrees drop 1
          gm(1) -> s(components.toList)
        } toMap
      case _ =>
        marks flatMap (gm => (new Reconstruct(gm(1), nonMatches) ++ im)(enc).headOption map (gm(1) -> _)) toMap;
    }
    
    val elab = (anchorScheme match {
      case Some(s) =>
        marks flatMap (gm => (new Reconstruct(gm(1), nonMatches) ++ im)(enc).headOption map ((_, subterms(gm(1))))) toList
      case _ => List.empty
    }) filter { case (x, y) => x != y }
    
    for (t <- subterms.values) println("    " + (t toPretty))

    // Get the associated tuples for any newly introduced terms
    val dir = new Tree[Trie.DirectoryEntry](-1, 1 until 5 map (new Tree[Trie.DirectoryEntry](_)) toList)  /* ad-hoc directory */
    val tuples = spanning(new Trie[Int](dir) ++= work0.trie.words, marks map (_(1)), s.tuples map (_(1)))
    s ++ marks ++ elab ++ tuples at subterms
  }
  
  def explore(s: State, rules: List[CompiledRule], anchor: Term) = {
    // Apply rules to find the pattern
    val work = new WorkLoop(s.tuples, rules, directory)
    work.stream() foreach (_ map println)
    println("-" * 60)

    val anchor_# = enc.ntor --> anchor
    val matches = work.matches(_phMarker.leaf) filter (_(2) == anchor_#)
    val nonMatches = work.nonMatches(_phMarker.leaf, _goalMarker.leaf)
    
    for (gm <- matches if gm(2) == anchor_#;
         t <- new Reconstruct(gm(1), nonMatches)(enc))
      println("    " + (t toPretty))
  }
  
  def generalize(s: State, rules: List[CompiledRule], leaves: List[Term], name: Option[Term], context: List[Term]): State = {
    // Apply rewrite rules until saturated    
    val work = new WorkLoop(s.tuples, rules, directory)
    work()
    println("-" * 60)
    val work0 = compaction(work)
    
    // Reconstruct and generalize
    val gen =
      for (gm <- work0.matches(_phMarker.leaf);
           t <- new Reconstruct(gm(1), work0.nonMatches(_phMarker.leaf, _goalMarker.leaf))(enc);
           tg <- generalize(t, leaves, context)) yield {
        println(s"    ${t.toPretty}")
        val vas = 0 until leaves.length map Strip.greek map (TV(_))
        println(s"    as  ${((vas ↦: tg) :@ leaves).toPretty}") // ${leaves map (_.toPretty) mkString " "}")
        //tg
        (s.focusedSubterm get gm(1) map ((_, t))) ++
        (name match {
           case Some(f) => List((t, f :@ leaves))
           case None => List((t, (vas ↦: tg) :@ leaves))
        })
      }
    
    s ++ gen.flatten //(name map ((_, gen.head)))
  }
  
  def elaborate(s: State, rules: List[CompiledRule], goal: CompiledGoal): State = {
    elaborate(s, rules ++ goal.rules, goal.scheme)
  }

  def elaborate(s: State, rules: List[CompiledRule], goalScheme: Scheme) = {
    val work = new WorkLoop(s.tuples, rules, directory)
    val nonMatches = work.nonMatches(_phMarker.leaf, _phmMarker.leaf, _goalMarker.leaf)
    work()
    println("-" * 60)
    val work0 = compaction(work)
    
    showem(work0.goalMatches, work.trie)
    
    work0.goalMatches.headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m, work0.trie).subtrees)
        val original =
          s.focusedSubterm get m(1) match {
            case Some(original) => original
            case _ => 
              new Reconstruct(m(1), nonMatches)(enc).headOption getOrElse TI("?")
          }
        println(s"${original toPretty} --> ${elaborated toPretty}")
        s ++ List((original, elaborated))
      case _ => s
    }
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
    
    val progf = new FileWriter("prog.json")
    val cc = new DisplayContainer
    val json = cc.map("program" -> state.program,
        "elaborate" -> (state.elaborate map (el => cc.list(List(el._1, el._2)))))
    progf.write(json.toString)
    progf.close()
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
      println(s"${gm mkString " "}")//  [${gm map (enc.ntor <--) mkString "] ["}]");
      for (ln <- transposeAll(gm.toList drop 2 map (x => new Reconstruct(x, trie)(enc).toList), B))
        println("    " + mkStringColumns(ln map (t => if (t == B) "" else t toPretty), 40 ))
    }
  }
  
  def pickFirst(match_ : Array[Int], trie: Trie[Int]) = {
    new Reconstruct(match_, trie)(enc).head
  }
  
  def generalize(t: Term, leaves: List[Term], context: List[Term]): Option[Term] =
    leaves.indexOf(t) match {
      case -1 => if (context contains t) None else T_?(t.root)(t.subtrees map (generalize(_, leaves, context)))
      case idx => Some( TI(Strip.greek(idx)) )
    }
  
  def T_?(root: Identifier)(subtrees: List[Option[Term]]) = 
    if (subtrees exists (_ == None)) None else Some(T(root)(subtrees map (_.get)))
  
  object NoDupRules1 extends Rules {
    import BasicSignature._
    import Rewrite.RuleOps
    val enc = NoDup.enc

    val ys = TI("ys")
    val vars = List(y, ys)
    val rulesSrc = List(
        xs =:= cons(`x'`, `xs'`),
        (_nodup:@(nil)) =:> TRUE,
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
        

  
}