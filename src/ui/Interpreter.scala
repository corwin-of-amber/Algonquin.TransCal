package ui

import ontopt.pen.Grammar

import syntax.AstSugar._
import syntax.Identifier
import syntax.Scheme
import syntax.Strip
import semantics.Namespace
import syntax.transform.TreeSubstitution
import semantics.LambdaCalculus
import relentless.rewriting.CompiledRule
import relentless.rewriting.Rewrite
import relentless.matching.Encoding
import relentless.rewriting.Revision
import relentless.rewriting.RevisionDiff
import relentless.rewriting.Locate
import examples.BasicSignature
import relentless.rewriting.Generalize
import relentless.rewriting.Elaborate
import relentless.rewriting.Rules



object Interpreter {
  
  import Parser._
  
  case class State(prog: Revision, rules: Rules)  {
    def !< (op: Revision => Revision) = State(op(prog), rules)
    def !> (op: Rules => Rules) = State(prog, op(rules))
    
    def +<>+ (op: Revision => (RevisionDiff, Rules)) = op(prog) match {
      case (p, r) => State(prog ++: p, rules ++ r)
    }
  }
  
	def main(args: Array[String])
	{
		val program = raw"""nodup ⟨⟩ = ⟨⟩
                       |nodup (?x :: ?xs) = (~(elem ?x ?xs) /\ nodup ?xs)
                       |_ /\ _ -> 1                             [only assoc]
                       |1 -> ?nodup' {x} xs
                       |xs = x' :: xs'
                       |1 -> _ /\ _ /\ _ /\ _
                       |x ∉ _ /\ x' ∉ _ -> _ ‖ _
                       |(_ ∪ _ ‖ _) /\ _ -> nodup' _ _  """.stripMargin.replace("\n", " ; \n") + " ; "
		
		println(program)
		
		val lex = new BabyLexer(TOKENS)
		val p = new Parser(new Grammar(GRAMMAR), NOTATIONS)
		val tokens = lex.tokenize(program)
		
		implicit val enc = examples.NoDup.enc
		implicit val directory = examples.NoDup.directory
		
		val interp = new Interpreter
		var state = State(Revision(examples.NoDup.nodupProg), Rules.empty)
		
		p(tokens).headOption match {
		  case Some(prog) => for (t <- prog) {
		    println("-" * 65)
		    val pat = interp.varify(t)
		    println(s"(${pat.vars mkString " "})  ${pat.template toPretty}")
		    for (a <- t.get[DeductionHints]) println(s"  ${a}")
		    state = interp.interpretDerivation(state, pat)
		  }
		  case None => println("oops! parse error")
		}
		
		println("=" * 65)
		for (r <- state.rules.src) println(s"• ${r.template.toPretty}")
		
		examples.NoDup.dump(state.prog)
	}
	
}
	
class Interpreter(implicit val enc: Encoding) {
  
  import Interpreter._
  
  val BasicRules = new examples.BasicRules
  val AssocRules = new examples.AssocRules
  
  def interpretDerivation(s: State, scheme: Scheme.Template) = {
    val t = scheme.template
    val vars = scheme.vars
    if (t.root == ->.root) {
      /**/ assume(t.subtrees.length == 2) /**/
      val List(from, to) = t.subtrees
      if (isAnchorName(to)) {
        println("  locate")
        s +<>+ new Locate(AssocRules.rules ++ s.rules.compiled,
                         examples.NoDup.mkLocator(vars map (T(_)):_*)(from, to)).apply
      }
      else {
        if (!isAnchorName(from)) println("  locate &")
        LambdaCalculus.isApp(to) match {
          case Some((f, args)) if f.isLeaf && (vars contains f.leaf) =>
            println(s"  generalize ${f} ${args}")
            //val ctx = Set(BasicSignature.x, BasicSignature.xs)
            s +<>+ new Generalize(BasicRules.rules, args, Some(f), None).apply
          case _ => 
            println("  elaborate")
            if (isAnchorName(from))
              s +<>+ new Elaborate(BasicRules.rules ++ s.rules.compiled,
                                 examples.NoDup.mkGoal(vars map (T(_)):_*)(to, Some(from))).apply
            else {
              val anchor = $TI("!")
              val fromvars = vars filter (from.terminals contains _)
              val tovars = vars filter (to.terminals contains _)
              s +<>+ new Elaborate(BasicRules.rules ++ s.rules.compiled, 
                examples.NoDup.mkLocator_simple(fromvars map (T(_)):_*)(from, anchor) ++
                examples.NoDup.mkGoal(tovars map (T(_)):_*)(to, Some(anchor))).apply
            }
        }
      }
    }
    else if (t.root == `=`) {
      println("  let")
      s !> (_ + scheme)
    }
    else {
      println(s"  unknown root '${t.root}'")
      s
    }
  }
  
  def isAnchorName(t: Term) = t.isLeaf && t.leaf.literal.isInstanceOf[Int]
  
  val PATVAR_RE = raw"\?(.*)".r
  
  def varify(t: Term) = {
    val `ⓧ` = "ⓧ"  /* this symbol is used as a placeholder */
    def varify0(t: Term): Scheme.Template = t.root.literal match {
      case PATVAR_RE(lit) => val v = T(new Identifier(lit, t.root.kind, t.root.ns)); new Scheme.Template(List(v.leaf), v)
      case "_" => val v = $TV(`ⓧ`); new Scheme.Template(List(v.leaf), v)
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