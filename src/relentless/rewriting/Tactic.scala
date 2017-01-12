package relentless.rewriting

import syntax.AstSugar.{Term, FormulaDisplay}
import relentless.matching.Encoding
import relentless.matching.Trie
import relentless.matching.Trie.Directory
import syntax.Tree
import relentless.rewriting.Rewrite.WorkLoop
import syntax.Scheme
import syntax.Strip



abstract class Tactic {
  def apply(r: Revision): Revision
}


case class Revision(val program: Term, val focusedSubterm: Map[Int, Term], val elaborate: List[(Term, Term)], val tuples: List[Array[Int]])(implicit val enc: Encoding, val directory: Directory) {
  def this(program: Term)(implicit enc: Encoding, directory: Directory) = this(program, Map.empty, List.empty, enc.toTuples(program))
  
  lazy val trie = new Trie[Int](directory) ++= tuples
  
  def at(subterms: Map[Int, Term]) = Revision(program, subterms, elaborate, tuples)
  
  def indexMapping: Map[Int, Term] = program.nodes ++ (elaborate flatMap (_._2.nodes)) map (t => (enc.ntor --> t, t)) toMap
  def incorporate(term: Term, as: Term) = enc.toTuples(term, as)

  def +(el: (Term, Term)) = Revision(program, focusedSubterm, elaborate :+ el, tuples ++ incorporate(el._2, el._1))
  def ++(els: Iterable[(Term, Term)]) = Revision(program, focusedSubterm, elaborate ++ els, tuples ++ (els flatMap (el => incorporate(el._2, el._1))))
  def ++(l: Iterable[Array[Int]])(implicit d: DummyImplicit) = Revision(program, focusedSubterm, elaborate, tuples ++ l)
  
  def +-(el: (Term, Term)) = Revision(program, focusedSubterm, elaborate :+ el, tuples)  // add but not incorporate: this is a bit weird, but makes sense if there is an appropriate rule in place
}

object Revision {
  def apply(program: Term)(implicit enc: Encoding, directory: Directory) = new Revision(program)
}


abstract class RuleBasedTactic(rules: List[CompiledRule]) extends Tactic {

  def work(s: Revision) = {
    val work0 = new WorkLoop(s.tuples, rules, s.directory)(s.enc)
    work0()
    println("-" * 60)
    work0
  }

}


object RuleBasedTactic {

  import syntax.AstSugar.$TI

  object Markers {  
    val goal = $TI("gem", "marker")
    val placeholder = $TI("⨀", "marker")
    val placeholderEx = $TI("⨀⋯", "marker")
    
    val all = List(goal, placeholder, placeholderEx)
  }
  
  class CompiledGoal(val rules: List[CompiledRule], val scheme: Scheme)
  
  class CompiledPattern(val rules: List[CompiledRule], val scheme: Option[Scheme], val anchor: Term) {
    def ++(goal: CompiledGoal) = new CompiledGoal(rules ++ goal.rules, goal.scheme)  // note: omits this.scheme
  }
  
  /**
   * Auxiliary function for finding all words incident (containing at locs >= 1) to init,
   * then all their neighbors (incident to those words' letters) recursively,
   * while not traversing across the boundary.
   */
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

}


trait Compaction extends RuleBasedTactic {

  import RuleBasedTactic.Markers
  
  override def work(s: Revision) = compaction(super.work(s))(s.enc)
  
  def compaction(work: WorkLoop)(implicit enc: Encoding): WorkLoop = {
    val trie = work.trie
    val equiv = collection.mutable.Map.empty[Int, Int]
    val except = Markers.all map (enc.ntor --> _.leaf)
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

}  


class Locate(rules: List[CompiledRule], anchor: Term, anchorScheme: Option[Scheme]) extends RuleBasedTactic(rules) {
  
  import RuleBasedTactic._
  
  def this(rules: List[CompiledRule], pattern: RuleBasedTactic.CompiledPattern) =
    this(rules ++ pattern.rules, pattern.anchor, pattern.scheme)

  def apply(s: Revision) = {
    val work0 = work(s)

    val anchor_# = s.enc.ntor --> anchor
    val marks = work0.matches(Markers.placeholder.leaf) filter (_(2) == anchor_#)
    val matches = work0.matches(Markers.placeholderEx.leaf) filter (_(2) == anchor_#)
    val nonMatches = work0.nonMatches(Markers.all map (_.leaf):_*)

    val enc = s.enc
    
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
  
}


class Generalize(rules: List[CompiledRule], leaves: List[Term], name: Option[Term], context: List[Term]) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._
  import syntax.AstSugar._
  
  def apply(s: Revision) = {
    val work0 = this.work(s)
    
    // Reconstruct and generalize
    val gen =
      for (gm <- work0.matches(Markers.placeholder.leaf);
           t <- new Reconstruct(gm(1), work0.nonMatches(Markers.all map (_.leaf):_*))(s.enc);
           tg <- examples.NoDup.generalize(t, leaves, context)) yield {
        println(s"    ${t.toPretty}")
        val vas = 0 until leaves.length map Strip.greek map (TV(_))
        println(s"    as  ${((vas ↦: tg) :@ leaves).toPretty}")

        (s.focusedSubterm get gm(1) map ((_, t))) ++
        (name match {
           case Some(f) => List((t, f :@ leaves), (f, (vas ↦: tg)))
           case None => List((t, (vas ↦: tg) :@ leaves))
        })
      }
    
    s ++ gen.flatten
  }

}


class Elaborate(rules: List[CompiledRule], goalScheme: Scheme) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._
  import syntax.AstSugar.TI
  
  def this(rules: List[CompiledRule], goal: RuleBasedTactic.CompiledGoal) =
    this(rules ++ goal.rules, goal.scheme)
  
  def apply(s: Revision) = {
    val work = this.work(s)

    val nonMatches = work.nonMatches(Markers.all map (_.leaf):_*)
    
    examples.NoDup.showem(work.matches(Markers.goal.leaf), work.trie)
    
    work.matches(Markers.goal.leaf).headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m, work.trie)(s.enc).subtrees)
        val original =
          s.focusedSubterm get m(1) match {
            case Some(original) => original
            case _ => 
              new Reconstruct(m(1), nonMatches)(s.enc).headOption getOrElse TI("?")
          }
        println(s"${original toPretty} --> ${elaborated toPretty}")
        s ++ List((original, elaborated))
      case _ => s
    }
  }

  def pickFirst(match_ : Array[Int], trie: Trie[Int])(implicit enc: Encoding) = {
    new Reconstruct(match_, trie)(enc).head
  }

}
