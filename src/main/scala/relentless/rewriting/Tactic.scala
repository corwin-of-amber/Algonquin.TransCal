package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature
import relentless.matching.{Encoding, Trie}
import relentless.matching.Trie.Directory
import semantics.LambdaCalculus
import syntax.AstSugar._
import syntax.{Identifier, Scheme, Strip, Tree}
import relentless.Utils


abstract class Tactic {
  def apply(r: Revision): (RevisionDiff, Rules)
}


case class Revision(val program: Term, val env: Revision.Environment, val focusedSubterm: Map[Int, Term], val elaborate: List[Revision.Equivalence], val tuples: List[BaseRewriteEdge[Int]])(implicit val enc: Encoding, val directory: Directory) {
  def this(program: Term)(implicit enc: Encoding, directory: Directory) = this(program, Revision.Environment.empty, Map.empty, List.empty, enc.toOriginalEdges(program))

  lazy val trie = new Trie[Int, BaseRewriteEdge[Int]](directory) ++= tuples

  def at(subterms: Map[Int, Term]) = Revision(program, env, subterms, elaborate, tuples)

  def incorporate(term: Term, as: Term) = enc.toOriginalEdges(term, as)

  import Revision._

  def ++(e: Environment) = Revision(program, Environment(env.vars ++ e.vars), focusedSubterm, elaborate, tuples)
  def +(el: Equivalence) = Revision(program, env, focusedSubterm, elaborate :+ el, tuples ++ incorporate(el.rhs, el.lhs))
  def ++(els: Iterable[Equivalence]) = Revision(program, env, focusedSubterm, elaborate ++ els, tuples ++ (els flatMap (el => incorporate(el.rhs, el.lhs))))
  def ++(l: Iterable[BaseRewriteEdge[Int]])(implicit d: DummyImplicit) = Revision(program, env, focusedSubterm, elaborate, tuples ++ l)

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
    val tuples_++     : List[BaseRewriteEdge[Int]]             /* corresponds to ++(List[Array[Int]]) */
    ) {

  def ++:(rev: Revision) = {
    val s = rev ++ Revision.Environment(env_++) ++ elaborate_++ ++- elaborate_++- ++ tuples_++
    terms match { case Some(terms) => s at terms case _ => s }
  }
}


case class Rules(rules: List[RewriteRule])(implicit val enc: Encoding) {
  def +(ruledef: RewriteRule) = {
    /**/ assert(enc ne Rules.dummyEncoding) /**/   /* the dummy encoding should never be changed */
    Rules(rules :+ ruledef)
  }

  def ++(other: Rules) = {
    /**/ assume(rules.isEmpty || other.rules.isEmpty || (enc eq other.enc)) /**/   /* seems like it doesn't make sense otherwise? */
    Rules(rules ++ other.rules)(if (rules.isEmpty) other.enc else enc)
  }
}

object Rules {
  lazy val dummyEncoding = new Encoding

  def empty(implicit enc: Encoding = dummyEncoding) = Rules(List.empty)
}


abstract class RuleBasedTactic(rules: List[RewriteRule]) extends Tactic with LazyLogging {

  def work(s: Revision) = {
    val work0 = new Rewriter(s.tuples, rules, s.directory)(s.enc)
    work0()
    logger.info("-" * 60)
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

  class CompiledGoal(val rules: List[RewriteRule], val scheme: Scheme)

  class CompiledPattern(val rules: List[RewriteRule], val scheme: Option[Scheme], val anchor: Term) {
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
    import relentless.rewriting.RewriteRule.RuleOps
    val term = anchor match {
      case None => pattern =:> Markers.goal(vars toList)
      case Some(anchor) => (Markers.placeholder(anchor) ||| pattern) =:> Markers.goal(vars toList)
    }
    val rewriteRule = RewriteRule(term, vars toList, RewriteRule.Category.Goal)
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledGoal(rewriteRule, tmpl)
  }

  def mkLocator(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding) = {
    import relentless.rewriting.RewriteRule.RuleOps
    // ⨀(anchor) is to mark the matched term as the goal
    // ⨀⋯(anchor, vars...) is for locate() to be able to reconstruct the term using tmpl
    val rewriteRule = RewriteRule(
        pattern =:> (Markers.placeholder(anchor) ||| Markers.placeholderEx(anchor :: vars.toList)),
        vars.toList,
        RewriteRule.Category.Locator
      )
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledPattern(rewriteRule, Some(tmpl), anchor)
  }

  def mkLocator_simple(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding) = {
    import relentless.rewriting.RewriteRule.RuleOps
    // ⨀(anchor) is to mark the matched term as the goal
    val rewriteRule = RewriteRule(pattern =:> Markers.placeholder(anchor), vars toList, RewriteRule.Category.Locator)
    new CompiledPattern(rewriteRule, None, anchor)
  }

  /**
   * Auxiliary function for finding all words incident (containing at locs >= 1) to init,
   * then all their neighbors (incident to those words' letters) recursively,
   * while not traversing across the boundary.
   */
  def spanning(trie: Trie[Int, BaseRewriteEdge[Int]], init: Iterable[Int], boundary: Iterable[Int]) = {
    import collection.mutable
    val ws = mutable.Set.empty ++ boundary
    val wq = mutable.Queue.empty ++ init
    Reconstructer.whileYield(wq.nonEmpty) {
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

  def compaction(work: Rewriter)(implicit enc: Encoding): Rewriter = compaction0(work)

  private def compaction0(work: Rewriter)(implicit enc: Encoding): Rewriter = {
    val trie = work.trie
    val equiv = collection.mutable.Map.empty[Int, Int]
    val except = Markers.all map (enc --> _.leaf)

    // for each edge type, compare words by parameters and add to equiv by target.
    for ((k, subtrie) <- trie.subtries(0) if !(except contains k)) {
      equiv ++= subtrie.uniques(2, (_.min))
    }
    /*--------------------
     * a meek effort to eliminate id() terms by equating the argument with the result
     */
    val id = enc --> BasicSignature._id.leaf
    // get all words with type id and find eqalities between target and single param
    for (subtrie <- trie.subtries(0).get(id); word <- subtrie.words) {
      if (word(1) != word(2)) {
        logger.trace(s"adding ${word(1)} and ${word(2)} to equiv")
        val rep = Seq(word(2), word(1)).min
        val source = Seq(word(2), word(1)).max
        equiv += source -> rep
      }
    }
    /*--------------------*/
    if (equiv.nonEmpty) {
      def subst(w: BaseRewriteEdge[Int]): BaseRewriteEdge[Int] = w match {
        case x: OriginalEdge[Int] => OriginalEdge(w map (x => equiv getOrElse (x,x)))
        case x: RewriteEdge[Int] => RewriteEdge(x.origin, w map (x => equiv getOrElse (x,x)))
      }
      val work = new Rewriter(trie.words.toStream /*filter (_(0) != id)*/ map subst, List.empty, trie.directory)
      work()
      compaction0(work)
    }
    else work
  }

}

/** Creates rules such that the given (by user) equality between terms will be represented in our
  * rewrite system.
  *
  * @param equalities Templates that represent the terms that are equal
  * @param incorporate
  */
class Let(equalities: List[Scheme.Template], incorporate: Boolean = false) extends Tactic {

  import LambdaCalculus.↦⁺
  import RuleBasedTactic.⇢
  import syntax.AstSugar._

  val vars = equalities flatMap (_.vars) map (T(_))
  def rules(implicit enc: Encoding) = {
    val rewrites: List[RewriteRule] = (equalities ++ derivedFrom(equalities)) map skolemize flatMap (RewriteRule(_, RewriteRule.Category.Definition))
    Rules(rewrites)
  }
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


/** Find a [[Term]] to focus on
  *
  * @param rules
  * @param anchor
  * @param anchorScheme
  */
class Locate(rules: List[RewriteRule], anchor: Term, anchorScheme: Option[Scheme]) extends RuleBasedTactic(rules) with Compaction {

  import RuleBasedTactic._

  def this(rules: List[RewriteRule], pattern: RuleBasedTactic.CompiledPattern) =
    this(rules ++ pattern.rules, pattern.anchor, pattern.scheme)

  def apply(s: Revision) = {
    val work0 = work(s)

    val anchor_# = s.enc --> anchor
    val anchordef = work0.matches(anchor.root)
    /** All places with placeholder where first parameter is anchor_# */
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
    val subterms = anchorScheme match {
      case Some(s) =>
        matches map { gm =>
          val components = new Reconstructer(gm, work0.trie)(enc, except).head.subtrees drop 1
          gm(1) -> s(components.toList)
        } toMap
      case _ =>
        marks flatMap (gm => new Reconstructer(gm(1), work0.trie)(enc, except).headOption map (gm(1) -> _)) toMap;
    }

    val elab = (anchorScheme match {
      case Some(s) =>
        marks flatMap (gm => new Reconstructer(gm(1), work0.trie)(enc, except).headOption map ((_, subterms(gm(1))))) toList
      case _ => List.empty
    }) collect { case (x, y) if x != y => x ⇢ y }

    for (t <- subterms.values) logger.info("    " + (t toPretty))

    // Get the associated tuples for any newly introduced terms
    val dir = new Tree[Trie.DirectoryEntry](-1, 1 until 5 map (new Tree[Trie.DirectoryEntry](_)) toList)  /* ad-hoc directory */
    val tuples = spanning(new Trie[Int, BaseRewriteEdge[Int]](dir) ++= work0.trie.words, marks map (_(1)), s.tuples map (_(1)))
    (RevisionDiff(Some(subterms), List(), elab, List(), anchordef ++ marks ++ tuples toList), Rules.empty)
  }

}


/** Creates a new function in the program.
  *
  * @param rules
  * @param leaves
  * @param name
  * @param context
  */
class Generalize(rules: List[RewriteRule], leaves: List[Term], name: Option[Term], context: Option[Set[Term]]) extends RuleBasedTactic(rules) with Compaction {

  import RuleBasedTactic._
  import syntax.AstSugar._

  def apply(s: Revision) = {
    val work0 = this.work(s)

    // ad-hoc trie for context resolution
    lazy val dir = new Tree[Trie.DirectoryEntry](-1, 2 until 5 map (new Tree[Trie.DirectoryEntry](_)) toList)  /* ad-hoc directory */
    lazy val trie = new Trie[Int, BaseRewriteEdge[Int]](dir) ++= work0.trie.words

    implicit val enc = s.enc

//    logger.debug(s"Showing encoding values: ${enc.ntor.mapped.toStream.sortBy(_._2).mkString(", ")}")

    // Reconstruct and generalize
    val gen =
      for (gm <- work0.matches(Markers.placeholder.leaf).toStream;
           t <- new Reconstructer(gm(1), work0.nonMatches(Markers.all map (_.leaf):_*))(s.enc);
           //x <- Some(println(s"[generalize] ${t.toPretty}"));
           tg <- generalize(t, leaves, context getOrElse /*grabContext(gm(1), trie) ++ */s.env.vars.toSet)) yield {
        logger.info(s"    ${t.toPretty}")
        val vas = 0 until leaves.length map Strip.greek map (TV(_))
        logger.info(s"    as  ${((vas ↦: tg) :@ leaves).toPretty}")

        (s.focusedSubterm get gm(1) map (_ ⇢ t)) ++
        (name match {
           case Some(f) => List(t ⇢ (f :@ leaves), f ⇢ (vas ↦: tg))
           case None => List(t ⇢ ((vas ↦: tg) :@ leaves))
        })
      }

    object F { def unapply(t: Term) = LambdaCalculus.isAbs(t) }
    val elab = gen.headOption.toSeq.flatten
    val rules = elab collect { case (f ⇢ F(vas, body)) => new Scheme.Template(vas map (_.leaf), (f :@ vas) =:= body) }

    (RevisionDiff(None, List(), elab.toList, List(), List()), Rules(rules.toList flatMap (RewriteRule(_, RewriteRule.Category.Definition))))
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


/** Trying to reach a goal from given rules.
  * 
  * @param rules
  * @param goalScheme
  */
class Elaborate(rules: List[RewriteRule], goalScheme: Scheme) extends RuleBasedTactic(rules) with Compaction {

  import RuleBasedTactic._
  import syntax.AstSugar.TI

  def this(rules: List[RewriteRule], goal: RuleBasedTactic.CompiledGoal) =
    this(rules ++ goal.rules, goal.scheme)

  def apply(s: Revision) = {
    val work = this.work(s)

    implicit val enc = s.enc
    val except = Markers.all map (_.leaf) toSet

    Utils.showem(work.matches(Markers.goal.leaf), work.trie)

    //lazy val nonMatches = WorkLoop.nonMatches(s.tuples, Markers.all map (_.leaf):_*)

    work.matches(Markers.goal.leaf).headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m, work.trie).subtrees)
        val original =
          s.focusedSubterm get m(1) match {
            case Some(original) => original
            case _ =>
              new Reconstructer(m(1), s.tuples)(s.enc, except).headOption getOrElse TI("?")
          }
        logger.info(s"${original toPretty} --> ${elaborated toPretty}")
        (RevisionDiff(None, List(), List(original ⇢ elaborated), List(), List()), Rules.empty)
      case _ => (RevisionDiff(None, List(), List(), List(), List()), Rules.empty)
    }
  }

  def pickFirst(match_ : BaseRewriteEdge[Int], trie: Trie[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) = {
    val except = Markers.all map (_.leaf) toSet;
    new Reconstructer(match_, trie)(enc, except).head
  }

}
