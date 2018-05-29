package ui

import java.io._

import com.typesafe.scalalogging.LazyLogging
import relentless.{AssocRules, BasicRules}
import syntax.AstSugar._
import syntax.Identifier
import syntax.Scheme
import syntax.Strip
import syntax.transform.TreeSubstitution
import semantics.Namespace
import semantics.LambdaCalculus
import synth.pods.TacticalError
import report.data.DisplayContainer
import report.data.SerializationContainer
import relentless.matching.Encoding
import relentless.rewriting.Revision
import relentless.rewriting.RevisionDiff
import relentless.rewriting.Locate
import relentless.rewriting.RuleBasedTactic
import relentless.rewriting.Generalize
import relentless.rewriting.Elaborate
import relentless.rewriting.Rules
import relentless.rewriting.Let
import ui.Parser.DeductionHints
import scala.pickling.json._


object Interpreter extends LazyLogging {
  
  import Parser._
  
  case class State(prog: Revision, rules: Rules)  {
    def !< (op: Revision => Revision) = State(op(prog), rules)
    def !> (op: Rules => Rules) = State(prog, op(rules))
    
    def +<>+ (op: Revision => (RevisionDiff, Rules))(implicit ann: List[Annotation]) = op(prog) match {
      case (p, r) => State(prog ++: decorate(p, ann), rules ++ r)
    }
  }
  

	import Revision.Equivalence
	
	implicit object WithAnnotationForEquivalence extends WithAnnotation[Revision.Equivalence] {
	  override def copyWith(e: Equivalence, l: List[Annotation]) =
	    new Equivalence(e.lhs, e.rhs) with Annotated[Equivalence] { override val annot = l }
	}
	
  implicit def annotate_(e: Equivalence) = e match {
    case a: Annotated[Equivalence] @unchecked /* it cannot really be anything else because of this: X in Annotated[X] */ => a
    case _ => implicitly[WithAnnotation[Equivalence]].copyWith(e, List.empty)
  }
	
  def decorate(diff: RevisionDiff, annotations: List[Annotation]) = {
    def f(el: List[Revision.Equivalence]) = el map (_ /++ annotations)
    if (annotations.isEmpty) diff /* optimization */
    else RevisionDiff(diff.terms, diff.env_++, f(diff.elaborate_++), f(diff.elaborate_++-), diff.tuples_++)
  }  
	
	def toJson(rev: Revision)(implicit cc: SerializationContainer) = {
	  def annots(e: Equivalence) = e.get[DeductionHints] flatMap (_.options)
  	cc.map("program" -> rev.program,
        "elaborate" -> (rev.elaborate map (el => cc.list(List(el.lhs, el.rhs, cc.list(annots(el)))))))
	}
	
	def dump(rev: Revision) = {
    val progf = new FileWriter("prog.json")
    implicit val cc = new DisplayContainer
    progf.write(toJson(rev).toString)
    progf.close()
  }
	
	/* -- scallop is super slow to load? -- *
  import org.rogach.scallop._

	class CommandLineArgs(args: Array[String]) extends ScallopConf(args) {
	  val filename = trailArg[String](required=false, default=Some("-"))
	  
	  def file() = new File(filename())
	}
	*/

  class CommandLineArgs(args: Array[String]) {
    val filename = () => if (args.length > 0) args(0) else "-"
    
    def file() = filename() match {
      case "-" => new InputStreamReader(System.in)
      case fn => new InputStreamReader(new FileInputStream(fn), "UTF-8")
    }
  }
	
	
	def main(args: Array[String])
	{
	  /*nodup ⟨⟩ = true
                       |nodup (?x :: ?xs) = (~(elem ?x ?xs) /\ nodup ?xs)
		val program = raw"""nodup = ((⟨⟩ ↦ true) / (?x :: ?xs ↦ ~(elem ?x ?xs) /\ nodup ?xs))   [++]
                       |_ /\ _ -> 1⃝                              [only assoc]
                       |1⃝ -> ?nodup' {x} xs  [above]
                       |xs = x' :: xs'
                       |1⃝ -> _ /\ _ /\ _ /\ _
                       |x ∉ _ /\ x' ∉ _ -> _ ‖ _
                       |(_ ∪ _ ‖ _) /\ _ -> nodup' _ _  """.stripMargin.replace("\n", " ; \n") + " ; " */

    logger.info("Starting new run")
		val cmd = new CommandLineArgs(args)
		val program = getBlocks(cmd.file()).mkString(" ;\n") + " ; ";

    logger.info(program)
		
		val lex = new BabyLexer(TOKENS)
		val p = new Parser()
		val tokens = lex.tokenize(program)
    logger.info("Tokens:\n" + (tokens map (_.toString()) mkString " "))
		logger.info(s"${tokens}")
		
		implicit val enc = new Encoding
		implicit val directory = new BasicRules directory
		
		val interp = new Interpreter
		var state: State = null
		val stack = collection.mutable.Stack.empty[State]
		val out = collection.mutable.ListBuffer.empty[State]
		
		singleOption(p(tokens)) match {
		  case Some(prog) => for (t <- prog) {
        logger.info("-" * 65)
		    /* First statement has to be an equality, and it sets the focused term */
		    if (state == null) {
		      if (t.root != `=`) throw new TacticalError(s"Must start with an equality; found ${t.toPretty}")
		      state = State(Revision(t.subtrees(0)), Rules.empty)
		    }
		    if      (t == TI("→")) stack push state
		    else if (t == TI("←")) state = stack pop
		    else if (t == TI("□")) out += state
		    else {
  		    /* Apply tactic */
  		    val subst = new TreeSubstitution(p.variables.values.toList map { case v => (v, v) }) // causes leaf nodes with the same identifier to become aliased
  		    val patv = interp.varify(t)
  		    val pat = new Scheme.Template(patv.vars, subst(patv.template))
          logger.info(s"(${pat.vars mkString " "})  ${pat.template toPretty}")
  		    for (a <- t.get[DeductionHints]) logger.info(s"  ${a}")
  		    implicit val ann = t.get[Annotation]
  		    implicit val dh = t.get[DeductionHints]
  		    state = state +<>+ interp.interpretDerivation(state, pat).apply
		    }
		  }
		  case None =>
        logger.info(s"oops! parse error; ${p.error}")
		}
		
		for (state <- out :+ state) {
      logger.info("=" * 65)
  		for (r <- state.rules.src) logger.info(s"• ${r.src.template.toPretty}")
  		for (e <- state.prog.elaborate) logger.info(s"${e.lhs.toPretty}  ⇢  ${e.rhs.toPretty}   [${e.get[DeductionHints] flatMap (_.options)  mkString " "}]")
  		
  		dump(state.prog)
		}
	}
	
	def singleOption[A](l: List[A]): Option[A] = l match {
	  case x :: xs => 
	    if (xs.nonEmpty) logger.info(">>> warning: ambiguous parse")
	    Some(x)
	  case _ => None
	}

}
	
class Interpreter(implicit val enc: Encoding) extends LazyLogging {
  
  import Interpreter._
  import RuleBasedTactic.{mkLocator, mkLocator_simple, mkGoal}

  val BasicRules = new BasicRules
  val AssocRules = new AssocRules
  
  def interpretDerivation(s: State, scheme: Scheme.Template)(implicit hints: List[DeductionHints]) = {
    val t = scheme.template
    val vars = scheme.vars
    val rules =
      if (hints contains "only assoc") AssocRules.rules
      else BasicRules.rules ++ s.rules.compiled
    if (t.root == ->.root) {
      /**/ assume(t.subtrees.length == 2) /**/
      val List(from, to) = t.subtrees
      if (isAnchorName(to)) {
        logger.info("  locate")
        new Locate(rules,
                   mkLocator(vars map (T(_)):_*)(from, to))
      }
      else {
        if (!isAnchorName(from)) logger.info("  locate &")
        LambdaCalculus.isApp(to) match {
          case Some((f, args)) if f.isLeaf && (vars contains f.leaf) =>
            logger.info(s"  generalize ${f} ${args}")
            if (isAnchorName(from))
              new Generalize(rules, args, Some(f), None)
            else {
              // TODO
              new Generalize(rules, args, Some(f), None)
              
            }
          case _ =>
            logger.info("  elaborate")
            if (isAnchorName(from))
              new Elaborate(rules,
                            mkGoal(vars map (T(_)):_*)(to, Some(from)))
            else {
              val anchor = $TI("⚓︎")
              val fromvars = vars filter (from.terminals contains _)
              val tovars = vars filter (to.terminals contains _)
              new Elaborate(rules, 
                mkLocator_simple(fromvars map (T(_)):_*)(from, anchor) ++
                mkGoal(tovars map (T(_)):_*)(to, Some(anchor)))
            }
        }
      }
    }
    else if (t.root == `=`) {
      logger.info("  let")
      new Let(List(scheme), incorporate = hints exists (_.options contains "++"))
    }
    else {
      throw new TacticalError(s"unknown root '${t.root}'")
    }
  }
  
  def isAnchorName(t: Term) = t.isLeaf && (t.leaf.literal match {
    case _: Int => true
    case s: String => s.contains("⃝")
    case _ => false
  })
  
  val PATVAR_RE = raw"\?(.*)".r
  
  def varify(t: Term) = {
    val `ⓧ` = "ⓧ"  /* this symbol is used as a placeholder */
    def varify0(t: Term): Scheme.Template = t.root.literal match {
      case PATVAR_RE(lit) => val v = T(new Identifier(lit, t.root.kind, t.root.ns)); new Scheme.Template(List(v.leaf), v)
      case "_" => val v = $TV(`ⓧ`); new Scheme.Template(List(v.leaf), v)
      case _ if t.isLeaf => new Scheme.Template(List(), t)
      case _ => 
        val s = t.subtrees map varify0
        new Scheme.Template(removeDups(s flatMap (_.vars)), T(t.root, s map (_.template)))
    }
    
    // Rename the ⓧs into readable names
    val proto = varify0(t)
    val ns = new Namespace
    val vars = proto.vars.zipWithIndex map { 
      case (v, i) => (T(v), if (v.literal == `ⓧ`) T(new Identifier(Strip.greek(i), v.kind, ns)) else T(v))
    }
    val subst = new TreeSubstitution(vars filter (z => z._1 != z._2))
    new Scheme.Template(vars map (_._2.leaf), subst(proto.template))
  }
  
  def removeDups[A](l: List[A]): List[A] = {
    def aux(seen: Set[A], l: List[A]): List[A] =
      l match { case x::xs => if (seen contains x) aux(seen, xs) else x::aux(seen + x, xs) 
                case Nil => Nil }
    aux(Set.empty, l)
  }
}