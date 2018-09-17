package ui

import java.io._

import com.typesafe.scalalogging.LazyLogging
import relentless.{AssocRules, BasicRules, ExistentialRules}
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
import relentless.rewriting.Revision.Environment
import relentless.rewriting._
import ui.Parser.DeductionHints



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
    else RevisionDiff(diff.program_++, diff.vars_++, f(diff.equivalences_++))
  }  
	
	def toJson(rev: Revision)(implicit cc: SerializationContainer) = {
	  def annots(e: Equivalence) = e.get[DeductionHints] flatMap (_.options)
  	cc.map("program" -> rev.program,
        "equivalences" -> (rev.equivalences map (el => cc.list(List(el.lhs, el.rhs, cc.list(annots(el)))))))
	}
	
	def dump(rev: Revision) = {
    val progf = new FileWriter("prog.json")
    implicit val cc = new DisplayContainer
    progf.write(toJson(rev).toString)
    progf.close()
  }

  def dumpRules(rules: Seq[RewriteRule])(implicit enc: Encoding): Unit = {
    val rulef = new FileWriter("rules")
    for (rule <- rules) {
      rule.dumpCompiled(rulef)
      rulef.write("\n")
    }
    rulef.close()
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
		implicit val directory = (new BasicRules).directory


		val interp = new Interpreter
		var state: State = State(Revision(List(), Environment.empty, List()), Rules.empty)
		val stack = new collection.mutable.Stack[State]  /* using Stack.empty confuses IntelliJ :( */
		val out = collection.mutable.ListBuffer.empty[State]

    dumpRules(interp.BasicRules.rules)

    singleOption(p(tokens)) match {
		  case Some(prog) => for (t <- prog) {
        logger.info("-" * 65)
		    if      (t == TI("→")) stack.push(state)
		    else if (t == TI("←")) state = stack.pop
		    else if (t == TI("□")) out += state
		    else {
  		    /* Apply tactic */
  		    val subst = new TreeSubstitution(p.variables.values.toList map (v => (v, v))) // causes leaf nodes with the same identifier to become aliased
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
  		for (r <- state.rules.rules) logger.info(s"• ${r.src.template.toPretty}")
  		for (e <- state.prog.equivalences) logger.info(s"${e.lhs.toPretty}  ⇢  ${e.rhs.toPretty}   [${e.get[DeductionHints] flatMap (_.options)  mkString " "}]")
  		
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
  import relentless.rewriting.RewriteRule.RuleOps

  val BasicRules = new BasicRules
  val AssocRules = new AssocRules
  val ExistentialRules = new ExistentialRules
  
  def interpretDerivation(s: State, scheme: Scheme.Template)(implicit hints: List[DeductionHints]) = {
    val t = scheme.template
    val vars = scheme.vars
    val rules =
      if (hints exists (_.options containsSlice Seq("only","assoc"))) AssocRules.rules
      else if(hints exists (_.options containsSlice "allow existentials")) ExistentialRules.rules ++ s.rules.rules
      else BasicRules.rules ++ s.rules.rules
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
        import semantics.LambdaCalculus._
        to match {
            // Sugar syntax of isApp
          case f @: args if f.isLeaf && (vars contains f.leaf) =>
                    logger.info(s"  generalize ${f} ${args}")
                    assert(isAnchorName(from)) // TODO
              new Generalize(rules, from, args, Some(f))
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
      // Make it a directional rule, if a direction option was specified
      val rule = if (hints exists (_.options contains "->")) new Scheme.Template(scheme.vars, t.subtrees(0) =:> t.subtrees(1))
                 else scheme
      new Let(List(rule), incorporate = hints exists (_.options contains "++"))
    }
    else {
      throw new TacticalError(s"unknown root '${t.root}'")
    }
  }

  def isAnchorName(t: Term) = t.isLeaf && (t.leaf.literal match {
    case _: Int => true
    case s: String => s.contains("⃝") || s.startsWith("(")
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