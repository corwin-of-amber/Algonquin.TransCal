package relentless.rewriting

import syntax.Identifier
import syntax.Tree
import syntax.AstSugar._
import syntax.Scheme
import syntax.Strip

import relentless.matching.Encoding
import relentless.matching.Trie
import relentless.matching.Trie.Directory
import relentless.rewriting.Rewrite.WorkLoop
import scala.collection.SetLike
import semantics.LambdaCalculus



abstract class Tactic {
  def apply(r: Revision): (RevisionDiff, Rules)
}


case class Revision(val program: Term, val env: Revision.Environment, val focusedSubterm: Map[Int, Term], val elaborate: List[Revision.Equivalence], val tuples: List[Array[Int]])(implicit val enc: Encoding, val directory: Directory) {
  def this(program: Term)(implicit enc: Encoding, directory: Directory) = this(program, Revision.Environment.empty, Map.empty, List.empty, enc.toTuples(program))
  
  lazy val trie = new Trie[Int](directory) ++= tuples
  
  def at(subterms: Map[Int, Term]) = Revision(program, env, subterms, elaborate, tuples)
  
  def indexMapping: Map[Int, Term] = program.nodes ++ (elaborate flatMap (_.rhs.nodes)) map (t => (enc.ntor --> t, t)) toMap
  def incorporate(term: Term, as: Term) = enc.toTuples(term, as)

  import Revision._
  
  def ++(e: Environment) = Revision(program, Environment(env.vars ++ e.vars), focusedSubterm, elaborate, tuples)
  def +(el: Equivalence) = Revision(program, env, focusedSubterm, elaborate :+ el, tuples ++ incorporate(el.rhs, el.lhs))
  def ++(els: Iterable[Equivalence]) = Revision(program, env, focusedSubterm, elaborate ++ els, tuples ++ (els flatMap (el => incorporate(el.rhs, el.lhs))))
  def ++(l: Iterable[Array[Int]])(implicit d: DummyImplicit) = Revision(program, env, focusedSubterm, elaborate, tuples ++ l)
  
  def +-(el: Equivalence) = Revision(program, env, focusedSubterm, elaborate :+ el, tuples)  // add but not incorporate: this is a bit weird, but makes sense if there is an appropriate rule in place
  def ++-(els: Iterable[Equivalence]) = Revision(program, env, focusedSubterm, elaborate ++ els, tuples)
}

object Revision {
  case class Equivalence(lhs: Term, rhs: Term)
  case class Environment(vars: List[Term])
  object Environment { val empty = Environment(List.empty) }
  def apply(program: Term)(implicit enc: Encoding, directory: Directory) = new Revision(program)
}


case class RevisionDiff(
    val terms: Option[Map[Int, Term]],               /* corresponds to Revision.at */
    val env_++        : List[Term],                  /* corresponds to ++(Environment) */
    val elaborate_++  : List[Revision.Equivalence],  /* corresponds to ++(List[Equivalence]) */
    val elaborate_++- : List[Revision.Equivalence],  /* corresponds to ++-(List[Equivalence]) */
    val tuples_++     : List[Array[Int]]             /* corresponds to ++(List[Array[Int]]) */
    ) {
  
  def ++:(rev: Revision) = {
    val s = rev ++ Revision.Environment(env_++) ++ elaborate_++ ++- elaborate_++- ++ tuples_++
    terms match { case Some(terms) => s at terms case _ => s }
  }
}


case class Rules(src: List[Scheme.Template], compiled: List[CompiledRule])(implicit val enc: Encoding) {
  def +(ruledef: Scheme.Template) = {
    /**/ assert(enc ne Rules.dummyEncoding) /**/   /* the dummy encoding should never be changed */
    Rules(src :+ ruledef, compiled ++ Rewrite.compileRule(ruledef))
  }

  def ++(other: Rules) = {
    /**/ assume(compiled.isEmpty || other.compiled.isEmpty || (enc eq other.enc)) /**/   /* seems like it doesn't make sense otherwise? */
    Rules(src ++ other.src, compiled ++ other.compiled)(if (compiled.isEmpty) other.enc else enc)
  }
}

object Rules {
  lazy val dummyEncoding = new Encoding
  
  def empty(implicit enc: Encoding = dummyEncoding) = Rules(List.empty, List.empty)
  
  def apply(src: List[Scheme.Template])(implicit enc: Encoding) =
    new Rules(src, src flatMap Rewrite.compileRule)
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
  
  implicit class ⇢(private val lhs: Term) extends AnyVal {
    def ⇢(rhs: Term) = Revision.Equivalence(lhs, rhs)
  }
  object ⇢ {
    def unapply(e: Revision.Equivalence) = e match { case Revision.Equivalence(lhs, rhs) => Some((lhs, rhs)) }
  }
  
  /**
   * Helpers for constructing sets of rules for locating sub-terms and goals.
   */

  def mkGoal(vars: Term*)(pattern: Term, anchor: Option[Term] = None)(implicit enc: Encoding) = {
    import Rewrite.RuleOps
    val rules = Rewrite.compileRules(vars toList, List(anchor match {
      case None => pattern =:> Markers.goal(vars toList)
      case Some(anchor) => (Markers.placeholder(anchor) ||| pattern) =:> Markers.goal(vars toList)
    }))
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledGoal(rules, tmpl)
  }
  
  def mkLocator(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding) = {
    import Rewrite.RuleOps
    // ⨀(anchor) is to mark the matched term as the goal
    // ⨀⋯(anchor, vars...) is for locate() to be able to reconstruct the term using tmpl
    val rules = Rewrite.compileRules(vars toList, List(pattern =:> (Markers.placeholder(anchor) ||| Markers.placeholderEx(anchor :: vars.toList))))
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledPattern(rules, Some(tmpl), anchor)
  }
  
  def mkLocator_simple(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding) = {
    import Rewrite.RuleOps
    // ⨀(anchor) is to mark the matched term as the goal
    val rules = Rewrite.compileRules(vars toList, List(pattern =:> Markers.placeholder(anchor)))
    new CompiledPattern(rules, None, anchor)
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
     * uniques() groups words in given trie by values at locations >= index,
     * then declares _(1) to be equivalent for all words in each group.
     */
    def uniques(trie: Trie[Int], index: Int) {
      if (index >= trie.subtries.length || trie.subtries(index) == null) {
        if (trie.words.length > 1) {
          val equals = trie.words map (_(1))
          val rep = equals.min  /* the representative is chosen to be the lowest indexed element */
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
    /*--------------------
     * a meek effort to eliminate id() terms by equating the argument with the result
     */
    val id = enc.ntor --> examples.BasicSignature._id.leaf
    for (subtrie <- trie.subtries(0).get(id); word <- subtrie.words) {
      println(word mkString " ")
      if (word(1) != word(2))
        equiv += word(1) -> word(2)
    }
    /*--------------------*/
    if (equiv.nonEmpty) {
      def subst(w: Array[Int]) = w map (x => equiv getOrElse (x,x))
      val work = new WorkLoop(trie.words.toStream /*filter (_(0) != id)*/ map subst, List.empty, trie.directory)
      work()
      compaction(work)
    }
    else work
  }

}  


class Let(equalities: List[Scheme.Template], incorporate: Boolean = false) extends Tactic {
  
  import syntax.AstSugar._
  import RuleBasedTactic.⇢
  import Rewrite.RuleOps
  import LambdaCalculus.↦⁺
  
  val vars = equalities flatMap (_.vars) map (T(_))
  def rules(implicit enc: Encoding) = Rules((equalities ++ derivedFrom(equalities)) map skolemize)
    //{ val d = derivedFrom(equalities) ; Rules(if (d.isEmpty) equalities else d.toList) }
  val elab = equalities map (_.template) collect { case T(`=`, List(lhs, rhs)) => lhs ⇢ rhs }
  
  def apply(s: Revision) = (
      if (incorporate) RevisionDiff(None, vars, elab, List(), List())
                  else RevisionDiff(None, vars, List(), elab, List()), 
      rules(s.enc)
  )
  
  object ST { def unapply(s: Scheme.Template) = Some((s.vars, s.template)) }
  
  def derivedFrom(equalities: Iterable[Scheme.Template]): Iterable[Scheme.Template] = equalities flatMap {
    case ST(vars, T(`=`, List(lhs, rhs@T(/, _)))) =>
      derivedFrom( rhs split / map (lhs =:= _) map (new Scheme.Template(vars, _)) )
      //rhs split / collect { case T(↦, List(pat, expr)) => (lhs :@ pat) =:= expr } map (new Scheme.Template(vars, _))
    case ST(vars, T(`=`, List(lhs, ↦⁺(va, body)))) => List(new Scheme.Template(vars, (lhs :@ va) =:= body))
    case _ => List()
  }
  
  def skolemize(equality: Scheme.Template) = equality match {
    case ST(vars, t@T(`=`, List(lhs, rhs))) => new Scheme.Template(vars filter lhs.terminals.contains, t)
    case st => st
  }
}


class Locate(rules: List[CompiledRule], anchor: Term, anchorScheme: Option[Scheme]) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._
  
  def this(rules: List[CompiledRule], pattern: RuleBasedTactic.CompiledPattern) =
    this(rules ++ pattern.rules, pattern.anchor, pattern.scheme)

  def apply(s: Revision) = {
    val work0 = work(s)

    val anchor_# = s.enc.ntor --> anchor
    val marks = work0.matches(Markers.placeholder.leaf) filter (_(2) == anchor_#)
    val matches = work0.matches(Markers.placeholderEx.leaf) filter (_(2) == anchor_#)
    //val nonMatches = work0.nonMatches(Markers.all map (_.leaf):_*)

    implicit val enc = s.enc
    val except = Markers.all map (_.leaf) toSet

    /*examples.NoDup.dump(work0.trie.words)
    for (r <- rules) {
      if (r.dbg != null) println(r.dbg.toPretty)
      for (v <- r.shards) println(v.tuples map (_.mkString(" ")))
      println
      println(r.conclusion.tuples map (_.mkString(" ")))
      println("-" * 60)
    }*/
    
    // choose the first term for each match
    val im = s.indexMapping
    val subterms = anchorScheme match {
      case Some(s) =>
        matches map { gm =>
          val components = (new Reconstruct(gm, work0.trie) ++ im)(enc, except).head.subtrees drop 1
          gm(1) -> s(components.toList)
        } toMap
      case _ =>
        marks flatMap (gm => (new Reconstruct(gm(1), work0.trie) ++ im)(enc, except).headOption map (gm(1) -> _)) toMap;
    }
    
    val elab = (anchorScheme match {
      case Some(s) =>
        marks flatMap (gm => (new Reconstruct(gm(1), work0.trie) ++ im)(enc, except).headOption map ((_, subterms(gm(1))))) toList
      case _ => List.empty
    }) collect { case (x, y) if x != y => x ⇢ y }
    
    for (t <- subterms.values) println("    " + (t toPretty))

    // Get the associated tuples for any newly introduced terms
    val dir = new Tree[Trie.DirectoryEntry](-1, 1 until 5 map (new Tree[Trie.DirectoryEntry](_)) toList)  /* ad-hoc directory */
    val tuples = spanning(new Trie[Int](dir) ++= work0.trie.words, marks map (_(1)), s.tuples map (_(1)))
    (RevisionDiff(Some(subterms), List(), elab, List(), marks ++ tuples toList), Rules.empty)
  }
  
}


class Generalize(rules: List[CompiledRule], leaves: List[Term], name: Option[Term], context: Option[Set[Term]]) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._
  import syntax.AstSugar._
  
  def apply(s: Revision) = {
    val work0 = this.work(s)

    // ad-hoc trie for context resolution
    lazy val dir = new Tree[Trie.DirectoryEntry](-1, 2 until 5 map (new Tree[Trie.DirectoryEntry](_)) toList)  /* ad-hoc directory */
    lazy val trie = new Trie[Int](dir) ++= work0.trie.words
    
    implicit val enc = s.enc
    
    // Reconstruct and generalize
    val gen =
      for (gm <- work0.matches(Markers.placeholder.leaf);
           t <- new Reconstruct(gm(1), work0.nonMatches(Markers.all map (_.leaf):_*))(s.enc);
           //x <- Some(println(s"[generalize] ${t.toPretty}"));
           tg <- generalize(t, leaves, context getOrElse /*grabContext(gm(1), trie) ++ */s.env.vars.toSet)) yield {
        println(s"    ${t.toPretty}")
        val vas = 0 until leaves.length map Strip.greek map (TV(_))
        println(s"    as  ${((vas ↦: tg) :@ leaves).toPretty}")

        (s.focusedSubterm get gm(1) map (_ ⇢ t)) ++
        (name match {
           case Some(f) => List(t ⇢ (f :@ leaves), f ⇢ (vas ↦: tg))
           case None => List(t ⇢ ((vas ↦: tg) :@ leaves))
        })
      }
    
    object F { def unapply(t: Term) = LambdaCalculus.isAbs(t) }
    val elab = gen.headOption.toSeq.flatten
    val rules = elab collect { case (f ⇢ F(vas, body)) => new Scheme.Template(vas map (_.leaf), (f :@ vas) =:= body) }
    
    (RevisionDiff(None, List(), elab.toList, List(), List()), Rules(rules.toList))
  }

  import syntax.AstSugar.↦
  
  def grabContext(anchor: Int, trie: Trie[Int])(implicit enc: Encoding) = {
    import collection.mutable
    val ws = mutable.Set.empty[Int]
    val wq = mutable.Queue.empty ++ Seq(anchor)
    val `↦_#` = enc.ntor --> ↦
    Reconstruct.whileYield(wq.nonEmpty) {
      val u = wq.dequeue
      if (ws add u) {
        val incident = 2 until trie.subtries.length flatMap (i => trie.get(i, u).toList flatMap (_.words))
        for (e <- incident; v <- Some(e(1)) if !(ws contains v)) wq enqueue v
        for (e <- incident if e(0) == `↦_#`) yield e(2)
      }
      else Seq.empty
    } 
    .flatten
    .toSet map enc.asTerm flatMap patternLeaves
  }
  
  def patternLeaves(t: Term): Seq[Term] = {
    if (t.isLeaf) Seq(t)
    else LambdaCalculus.isApp(t) match {
      case Some((_, args)) => args flatMap patternLeaves
      case _ => t.subtrees flatMap patternLeaves
    }
  }
  
  def generalize(t: Term, leaves: List[Term], context: Set[Term]): Option[Term] = {
    //println(s"[generalize] ${t.toPretty}  with context  ${context}")

    leaves.indexOf(t) match {
      case -1 => if (context contains t) None else T_?(t.root)(t.subtrees map (generalize(_, leaves, context)))
      case idx => Some( TI(Strip.greek(idx)) )
    }
  }
  
  /** Construct Some[Term] only if no subtree is None. Otherwise, None. */
  def T_?(root: Identifier)(subtrees: List[Option[Term]]) = 
    if (subtrees exists (_ == None)) None else Some(T(root)(subtrees map (_.get)))

}


class Elaborate(rules: List[CompiledRule], goalScheme: Scheme) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._
  
  def this(rules: List[CompiledRule], goal: RuleBasedTactic.CompiledGoal) =
    this(rules ++ goal.rules, goal.scheme)
  
  def apply(s: Revision) = {
    val work = this.work(s)

    implicit val enc = s.enc
    val except = Markers.all map (_.leaf) toSet
    
    examples.NoDup.showem(work.matches(Markers.goal.leaf), work.trie)
    
    //lazy val nonMatches = WorkLoop.nonMatches(s.tuples, Markers.all map (_.leaf):_*)
    
    work.matches(Markers.goal.leaf).headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m, work.trie).subtrees)
        val original =
          s.focusedSubterm get m(1) match {
            case Some(original) => original
            case _ => 
              new Reconstruct(m(1), s.tuples)(s.enc, except).headOption getOrElse TI("?")
          }
        println(s"${original toPretty} --> ${elaborated toPretty}")
        (RevisionDiff(None, List(), List(original ⇢ elaborated), List(), List()), Rules.empty)
      case _ => (RevisionDiff(None, List(), List(), List(), List()), Rules.empty)
    }
  }

  def pickFirst(match_ : Array[Int], trie: Trie[Int])(implicit enc: Encoding) = {
    val except = Markers.all map (_.leaf) toSet;
    new Reconstruct(match_, trie)(enc, except).head
  }

}


class UnifyHole(given: Scheme.Template) extends syntax.Unify {
  override def isVar(x: Tree[Identifier]) = x.isLeaf && given.vars.contains(x.leaf)
  
  def apply(t: Term) = {
    try {
      makeMgu(given.template, t, List())
      true
    } catch  {
      case ex : syntax.Unify.CannotUnify => false
    }
  }
}

class FindRecursion(rules: List[CompiledRule], given: Scheme.Template, over: Term, disallowed: Set[Term]) extends RuleBasedTactic(rules) with Compaction {
  
  import RuleBasedTactic._

  val hole = TI("□")
  def matches = new UnifyHole(given)
  
  def apply(s: Revision): (RevisionDiff, Rules) = {
    val work0 = this.work(s)
    
    implicit val enc = s.enc
    
    println(s"Given ${given.template.toPretty} over ${over.toPretty}");
    
    val gen =
      for (gm <- work0.matches(Markers.placeholder.leaf);
           t <- new Reconstruct(gm(1), work0.nonMatches(Markers.all map (_.leaf):_*))(s.enc);
           (context, matched) <- findrec(t) if !matched.isEmpty) yield {
        println(s"    Context: ${context.toPretty}")
        println(s"    Matched: ${matched.map(_.toPretty).mkString("\n            ")}\n")
        val Some(original) = s.focusedSubterm get gm(1)
        (original ⇢ t)
      }
    (RevisionDiff(None, List(), List(gen(0)), List(), List()), Rules.empty)
  }
  
  def findrec(t: Term) : Option[(Term, List[Term])] = {
    if (matches(t)) {
      Some (hole, List(t))
    } 
    else if (disallowed.contains(t))
      None
    else {
      val submatches: List[Option[(Term, List[Term])]] = t.subtrees.map(findrec _)
      if (submatches.contains(None)) {
        None
      } else {
        val (lefts, rights) = submatches.flatten.unzip
        Some (new Tree(t.root, lefts), rights.flatten)
      }
    }
  }

}
