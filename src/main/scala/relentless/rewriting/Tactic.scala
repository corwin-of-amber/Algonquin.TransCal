package relentless.rewriting

import com.typesafe.scalalogging.LazyLogging
import relentless.{BasicSignature, Utils}
import relentless.matching.structures.vocabulary.Vocabulary.Directory
import relentless.matching.structures.vocabulary.Vocabulary
import relentless.matching.Encoding
import semantics.LambdaCalculus
import syntax.AstSugar._
import syntax.Scheme.Arity
import syntax.{Identifier, Scheme, Strip, Tree}


abstract class Tactic {
  def apply(r: Revision): (RevisionDiff, Rules)
}


case class Revision(program: List[Term], env: Revision.Environment, equivalences: List[Revision.Equivalence])(implicit val enc: Encoding, val directory: Directory) {
  def this(program: Term)(implicit enc: Encoding, directory: Directory) = this(List(program), Revision.Environment.empty, List.empty)

  def ++(terms: List[Term]): Revision = if (terms.isEmpty) this else new Revision(program ++ terms, env, equivalences)
  def ++(eqs: List[Revision.Equivalence])(implicit d: DummyImplicit): Revision =
    if (eqs.isEmpty) this else new Revision(program, env, equivalences ++ eqs)

  def declare(vars: List[Term]): Revision = if (vars.isEmpty) this else {
    assert(vars.forall(_.isLeaf)); new Revision(program, new Revision.Environment(env.vars ++ vars), equivalences)
  }

  lazy val asHyperEdges: List[OriginalEdge[Int]] = (program flatMap enc.toOriginalEdges) ++
    (for (eq <- equivalences) yield enc.toOriginalEdges(eq.lhs) ++ enc.toOriginalEdges(eq.rhs, eq.lhs)).flatten
}

object Revision {
  case class Equivalence(lhs: Term, rhs: Term)
  case class Environment(vars: List[Term])
  object Environment { val empty = Environment(List.empty) }
  def apply(program: Term)(implicit enc: Encoding, directory: Directory) = new Revision(program)
}


case class RevisionDiff( program_++ : List[Term],
                         vars_++ : List[Term],
                         equivalences_++ : List[Revision.Equivalence]) {

  def ++:(rev: Revision): Revision = (rev ++ program_++ ++ equivalences_++).declare(vars_++)
}

object RevisionDiff {
  val empty = RevisionDiff(List(), List(), List())
}


case class Rules(rules: List[RewriteRule])(implicit val enc: Encoding) {
  def +(ruledef: RewriteRule): Rules = {
    /**/ assert(enc ne Rules.dummyEncoding) /**/   /* the dummy encoding should never be changed */
    Rules(rules :+ ruledef)
  }

  def ++(other: Rules): Rules = {
    /**/ assume(rules.isEmpty || other.rules.isEmpty || (enc eq other.enc)) /**/   /* seems like it doesn't make sense otherwise? */
    Rules(rules ++ other.rules)(if (rules.isEmpty) other.enc else enc)
  }
}

object Rules {
  lazy val dummyEncoding = new Encoding

  def empty(implicit enc: Encoding = dummyEncoding) = Rules(List.empty)
}


abstract class RuleBasedTactic(rules: List[RewriteRule]) extends Tactic with LazyLogging {

  def work(s: Revision): Rewriter = {
    val work0 = new Rewriter(s.asHyperEdges, rules, s.directory)(s.enc)
    work0()
    logger.info("-" * 60)
    Utils.dump(work0.trie.getWords, "peg-before")(s.enc)
    work0
  }

}


object RuleBasedTactic {

  import syntax.AstSugar.$TI

  val noop: (RevisionDiff, Rules) = (RevisionDiff.empty, Rules.empty)

  object Markers {
    val goal: Tree[Identifier] = $TI("⊡", "marker")
    val placeholder: Tree[Identifier] = $TI("⨀", "marker")
    val placeholderEx: Tree[Identifier] = $TI("⨀⋯", "marker")

    val all = List(goal, placeholder, placeholderEx)
  }

  class CompiledGoal(val rules: List[RewriteRule], val scheme: Scheme)

  class CompiledPattern(val rules: List[RewriteRule], val scheme: Option[Scheme with Arity], val anchor: Term) {
    def ++(goal: CompiledGoal) = new CompiledGoal(rules ++ goal.rules, goal.scheme)  // note: omits this.scheme
  }

  implicit class ⇢(private val lhs: Term) extends AnyVal {
    def ⇢(rhs: Term) = Revision.Equivalence(lhs, rhs)
  }
  object ⇢ {
    def unapply(e: Revision.Equivalence): Some[(Term, Term)] = e match { case Revision.Equivalence(lhs, rhs) => Some((lhs, rhs)) }
  }

  /**
   * Helpers for constructing sets of rules for locating sub-terms and goals.
   */

  def mkGoal(vars: Term*)(pattern: Term, optionalAnchor: Option[Term] = None)(implicit enc: Encoding): CompiledGoal = {
    import relentless.rewriting.RewriteRule.RuleOps
    val term = optionalAnchor match {
      case None => pattern =:> Markers.goal(vars toList)
      case Some(anchor) => (anchor ||| pattern) =:> Markers.goal(vars toList)
    }
    val rewriteRule = RewriteRule(term, vars toList, RewriteRule.Category.Goal)
    val tmpl = new Scheme.Template(vars:_*)(pattern)
    new CompiledGoal(rewriteRule, tmpl)
  }

  def mkLocator(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding): CompiledPattern = {
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

  def mkLocator_simple(vars: Term*)(pattern: Term, anchor: Term)(implicit enc: Encoding): CompiledPattern = {
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
//  def spanning(trie: Vocabulary[Int, BaseRewriteEdge[Int]], init: Iterable[Int], boundary: Iterable[Int]): Stream[BaseRewriteEdge[Int]] = {
//    import collection.mutable
//    val ws = mutable.Set.empty ++ boundary
//    val wq = mutable.Queue.empty ++ init
//    Reconstructer.whileYield(wq.nonEmpty) {
//      val u = wq.dequeue
//      if (ws add u) {
//        val incident = 1 until trie.subtriesSize flatMap (i => trie.get(i, u).toList flatMap (_.getWords))
//        for (e <- incident; v <- e drop 1 if !(ws contains v)) wq enqueue v
//        incident
//      }
//      else Seq.empty
//    } flatten
//  }

}


trait Compaction extends RuleBasedTactic {

  import RuleBasedTactic.Markers

  override def work(s: Revision): Rewriter = compaction(super.work(s))(s.enc)

  def compaction(work: Rewriter)(implicit enc: Encoding): Rewriter = compaction0(work)

  private def compaction0(work: Rewriter)(implicit enc: Encoding): Rewriter = {
    val trie = work.trie
    val equiv = collection.mutable.Map.empty[Int, Int]
    val except = Markers.all map (enc --> _.leaf)

    // for each edge type, compare words by parameters and add to equiv by target.
    for ((k, subtrie) <- trie.firstSubtrie if !(except contains k)) {
      equiv ++= subtrie.uniques(2, _.min)
    }
    /*--------------------
     * a meek effort to eliminate id() terms by equating the argument with the result
     */
    val id = enc --> BasicSignature._id.leaf
    // get all words with type id and find eqalities between target and single param
    for (subtrie <- trie.firstSubtrie.get(id); word <- subtrie.getWords) {
      if (word(1) != word(2)) {
        val rep = Seq(word(2), word(1)).min
        val source = Seq(word(2), word(1)).max
        equiv += source -> rep
      }
    }
    /*--------------------*/
    if (equiv.nonEmpty) {
      def subst(w: BaseRewriteEdge[Int]): BaseRewriteEdge[Int] = w match {
        case _: OriginalEdge[Int] => OriginalEdge(w map (x => equiv getOrElse (x,x)))
        case x: RewriteEdge[Int] => RewriteEdge(x.origin, w map (x => equiv getOrElse (x,x)))
      }
      val work = new Rewriter(trie.toStream filter (_.edgeType != id) map subst, List.empty, trie.getDirectory)
      work()
      compaction0(work)
    }
    else work
  }

}

class Let(equalities: List[Scheme.Template], incorporate: Boolean = false) extends Tactic {

  import LambdaCalculus.↦⁺
  import RuleBasedTactic.⇢
  import syntax.AstSugar._

  val declaredVars: List[Tree[Identifier]] = equalities flatMap (_.vars) map (T(_))
  def rules(implicit enc: Encoding): Rules = {
    val rewrites: List[RewriteRule] = (equalities ++ derivedFrom(equalities)) map skolemize flatMap (RewriteRule(_, RewriteRule.Category.Definition))
    Rules(rewrites)
  }

  val elab: List[Revision.Equivalence] = equalities map (_.template) collect { case T(`=`, List(lhs, rhs)) => lhs ⇢ rhs }

  def apply(s: Revision): (RevisionDiff, Rules) = (
    RevisionDiff(if (incorporate) elab map (_.rhs) else List(), declaredVars, List()),
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

  def skolemize(equality: Scheme.Template): Scheme.Template = equality match {
    case ST(vars, t@T(`=`, List(lhs, _))) => new Scheme.Template(vars filter lhs.terminals.contains, t)
    case st => st
  }
}


class Locate(rules: List[RewriteRule], anchor: Term, anchorScheme: Option[Scheme with Arity]) extends RuleBasedTactic(rules) with Compaction {

  import RuleBasedTactic._

  def this(rules: List[RewriteRule], pattern: RuleBasedTactic.CompiledPattern) =
    this(rules ++ pattern.rules, pattern.anchor, pattern.scheme)

  def apply(s: Revision): (RevisionDiff, Rules) = {
    val work0 = work(s)

    /** Hyperterms representing the anchor (ideally, there should be exactly one) */
    val anchor_# = work0.matches(anchor.leaf) map (_.target)
    /** All placeholder hyperedges where first parameter is in anchor_# */
    val marks = work0.matches(Markers.placeholder.leaf) filter (anchor_# contains _(2))
    val matches = work0.matches(Markers.placeholderEx.leaf) filter (anchor_# contains _(2))
    //val nonMatches = work0.nonMatches(Markers.all map (_.leaf):_*)

    implicit val enc: Encoding = s.enc
    val except = Markers.all map (_.leaf) toSet

    /*examples.NoDup.dump(work0.trie.words)
    for (r <- rules) {
      if (r.dbg != null) println(r.dbg.toPretty)
      for (v <- r.shards) println(v.tuples map (_.mkString(" ")))
      println
      println(r.conclusion.tuples map (_.mkString(" ")))
      println("-" * 60)
    }*/

    Utils.dump(work0.trie.getWords)

    def reconstruct(root: Int /* hyper-node */) = new Reconstructer(root, work0.trie).apply(enc, except)

    def warnIfEmpty[A](root: Int, s: Stream[A]) = {
      if (s.isEmpty) logger.warn(s"[internal] warning: in Locate, node has no term representation ($root)")
      s
    }

    def selectFirstOfEach[A](alts: Seq[Seq[A]]) =
      try {
        alts map (_.head)
      }
      catch { case e: NoSuchElementException => assert(false); throw e }


    anchorScheme match {
      case Some(scheme) =>
        // choose the first term for each match for the scheme parameters
        val allAlternatives =
          for (edge <- matches) yield {
            val components = edge.params.drop(1)  // (the first parameter would be the marker)
            assert(components.length == scheme.arity)
            val alts = for (root <- components) yield warnIfEmpty(root, reconstruct(root))
            for (line <- Utils.formatColumnsPretty(alts)) logger.info(s"    $line")
            alts
          }
        allAlternatives.headOption match {
          case None => logger.warn("locate: no match"); noop
          case Some(h) =>
            val selected = selectFirstOfEach(h)
            val equiv = anchor ⇢ scheme(selected.toList)
            logger.info(equiv.toString)
            (RevisionDiff(List(), List(), List(equiv)), Rules.empty)
        }
      case None =>
        /* locate w/o a scheme will just display terms, but won't select anything */
        logger.warn("[internal] warning: in Locate, anchorScheme is missing for pattern. no terms will be returned.")
        for (edge <- marks) {
          val terms = reconstruct(edge.target)
          for (term <- terms)
            println(s"   ${term.toPretty}")
        }
        noop
    }
  }

}


class Generalize(rules: List[RewriteRule], anchor: Term, leaves: List[Term], name: Option[Term]) extends RuleBasedTactic(rules) with Compaction {

  import Generalize._
  import RuleBasedTactic._
  import syntax.AstSugar._

  def apply(s: Revision): (RevisionDiff, Rules) = {
    val work0 = this.work(s)

    val roots = work0.matches(anchor.leaf) map (_.target)

    implicit val enc: Encoding = s.enc
    val except = (Markers.all :+ anchor) map (_.leaf) toSet

    Utils.dump(work0.trie.getWords)

    val context = s.env.vars.toSet

    // Reconstruct and generalize
    val gen =
      for (root <- roots) yield {
        logger.info(root.toString)
        (for (term <- new Reconstructer(root, work0.trie).apply(enc, except);
             genTerm <- generalize(term, leaves, context)) yield {
          logger.info(s"    ${term.toPretty}")
          val vas = leaves.indices map Strip.greek map (TV(_))
          logger.info(s"    as  ${((vas ↦: genTerm) :@ leaves).toPretty}")

          val (equivs, ruleDefs) =
            name match {
              case Some(f) => (List(term ⇢ (f :@ leaves), f ⇢ (vas ↦: genTerm)), List((f :@ vas) =:= genTerm))
              case None =>
                logger.warn("[internal] warning: in Generalize, name is not specified. no rules will be generated.")
                (List(term ⇢ ((vas ↦: genTerm) :@ leaves)), List())
            }
          DerivedDefinition(vas, equivs, ruleDefs)
        }).take(NUM_ALTS_TO_SHOW)
      }

    /* select just the first generalization */
    gen.flatten.headOption match {
      case None => logger.warn("generalize: no match"); noop
      case Some(derivedDef) =>
        for (e <- derivedDef.equivs)
          println(e)
        val varIds = derivedDef.vars.toList map (_.leaf)
        val rules = new Let((derivedDef.ruleDefs map (new Scheme.Template(varIds, _))).toList).rules
        (RevisionDiff(List(), derivedDef.vars.toList, derivedDef.equivs.toList), rules)
    }
  }

  /*
  import syntax.AstSugar.↦

  def grabContext(anchor: Int, trie: Trie[Int, HyperEdge[Int]])(implicit enc: Encoding) = {
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
  }*/
}

object Generalize {

  final val NUM_ALTS_TO_SHOW = 10

  def generalize(t: Term, leaves: List[Term], context: Set[Term]): Option[Term] = {
    //println(s"[generalize] ${t.toPretty}  with context  ${context}")

    leaves.indexOf(t) match {
      case -1 => if (context contains t) None else T_?(t.root)(t.subtrees map (generalize(_, leaves, context)))
      case idx => Some( TI(Strip.greek(idx)) )
    }
  }

  /** Construct Some[Term] only if all subtrees are defined. Otherwise, None. */
  def T_?(root: Identifier)(subtrees: List[Option[Term]]): Option[Tree[Identifier]] =
    if (subtrees contains None) None else Some(T(root)(subtrees map (_.get)))

  case class DerivedDefinition(vars: Seq[Term], equivs: Seq[Revision.Equivalence], ruleDefs: Seq[Term])
}


class Elaborate(rules: List[RewriteRule], goalScheme: Scheme) extends RuleBasedTactic(rules) with Compaction {

  import RuleBasedTactic._
  import syntax.AstSugar.TI

  def this(rules: List[RewriteRule], goal: RuleBasedTactic.CompiledGoal) =
    this(rules ++ goal.rules, goal.scheme)

  def apply(s: Revision): (RevisionDiff, Rules) = {
    val work = this.work(s)

    implicit val enc: Encoding = s.enc
    val except = Markers.all map (_.leaf) toSet

    Utils.dump(work.trie.getWords)

    showAlternatives(work.matches(Markers.goal.leaf), work.trie)

    //lazy val nonMatches = WorkLoop.nonMatches(s.tuples, Markers.all map (_.leaf):_*)

    work.matches(Markers.goal.leaf).headOption match {
      case Some(m) =>
        val elaborated = goalScheme(pickFirst(m, work.trie).subtrees)
        val original =
          new Reconstructer(m(1), s.asHyperEdges)(s.enc, except).headOption getOrElse TI("?")
        logger.info(s"${original.toPretty} --> ${elaborated.toPretty}")
        (RevisionDiff(List(), List(), List(original ⇢ elaborated)), Rules.empty)
      case _ => (RevisionDiff(List(), List(), List()), Rules.empty)
    }
  }

  def showAlternatives(matches: Seq[BaseRewriteEdge[Int]], trie: Vocabulary[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding) {
    import semantics.Prelude.B

    val except = Markers.all map (_.leaf) toSet

    for (gm <- matches) {
      logger.info(s"${gm mkString " "}")//  [${gm map (enc.ntor <--) mkString "] ["}]");
      for (ln <- Utils.transposeAll(gm.toList drop 2 map (x => new Reconstructer(x, trie)(enc, except).toList), B))
        logger.info("    " + Utils.mkStringColumns(ln map (t => if (t == B) "" else t toPretty), 40 ))
    }
  }


  def pickFirst(match_ : BaseRewriteEdge[Int], trie: Vocabulary[Int, BaseRewriteEdge[Int]])(implicit enc: Encoding): Term = {
    val except = Markers.all map (_.leaf) toSet

    new Reconstructer(match_, trie)(enc, except).head
  }

}

