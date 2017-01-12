package ui

import java.util.regex.Pattern
import ontopt.pen.Word
import scala.collection.mutable.ListBuffer
import ontopt.pen.SimpleSentence
import ontopt.pen.EarleyParser
import ontopt.pen.Grammar
import collection.JavaConversions._
import scala.util.matching.Regex
import syntax.Tree
import syntax.AstSugar._
import examples.BasicSignature
import scala.reflect.ClassTag


  
object Parser {

  val GRAMMAR = 
		raw"""P -> | P S
		      S -> E ;
          S -> E [...] ;
          E -> E100
          E100     -> N: | E99
              N:   -> E99 : E100
          E99      -> N→ | E95
              N→  -> E95 -> E99
          E95      -> N↔︎ | E85
              N↔︎  -> E95 <-> E85
          E85      -> N∨ | E80
              N∨   -> E85 \/ E80 | E85 ∨ E80
          E80      -> N∧ | E75
              N∧   -> E80 /\ E75 | E80 ∧ E75
          E75      -> N¬ | E70
              N¬   -> ~ E75 | ¬ E75
          E70      -> N= | N≠ | N∈ | N∉ | N‖ | E60
              N=   -> E70 = E60
              N≠   -> E70 ≠ E60
              N∈   -> E70 ∈ E60
              N∉   -> E70 ∉ E60
              N‖   -> E70 || E60 | E70 ‖ E60
          E60      -> N:: | E50
              N::  -> E50 :: E60
          E50      -> N+ | N- | N∪ | E10
              N+   -> E50 + E10
              N-   -> E50 - E10
              N∪   -> E50 ∪ E10
          E10      -> N@ | E0
              N@   -> E10 E0
          E0       -> N{} | N() | N⟨⟩ | # | §
              N()  -> ( E100 )
              N{}  -> { E100 }
              N⟨⟩   -> ⟨ ⟩"""
	
	val TOKENS = List(raw"\d+".r -> "#",   // numeral
	                  raw"[?]?[\w'_]+".r -> "§",   // identifier
	                  raw"\[.*?\]".r -> "[...]",   // hints
	                  "[(){}+-=≠~<>:∈∉∪‖⟨⟩]".r -> "",
	                  raw"\\/|/\\|\|\||->|<->|::".r -> "",
	                  raw"\s+".r -> null)

	
	def op(op: => Term) = ((l: List[Term]) => op)
	def op(op: Term => Term) = ((l: List[Term]) => op(l(0)))
  def op(op: (Term, Term) => Term) = ((l: List[Term]) => op(l(0), l(1)))
  
  val NOTATIONS: Map[String, List[Term] => Term] = Map(
      "N:"   -> op(_ :- _),
      "N→"   -> op(_ -> _),
      "N↔︎"   -> op(_ <-> _),
      "N∧"   -> op(_ & _),
      "N∨"   -> op(_ | _),
      "N¬"   -> op(~_),
      "N="   -> op(_ =:= _),
      "N≠"   -> op(BasicSignature.!=:= _),
      "N::"  -> op(BasicSignature.cons _),
      "N‖"   -> op(BasicSignature.set_disj _),
      "N∈"   -> op(BasicSignature.in _),
      "N∉"   -> op(BasicSignature.not_in _),
      "N∪"   -> op(BasicSignature.set_union _),
      "N@"   -> op(_ :@ _),
      "N()"  -> op(x => x),
      "N{}"  -> op(BasicSignature.`{}` _),
      "N⟨⟩"   -> op(BasicSignature._nil)
  )
  
	def main(args: Array[String])
	{
		val program = raw"""_ /\ _ -> 1;                1 -> ?nodup' x y;       xs = x :: xs';          1 -> _ /\ _ /\ _ /\ _;
                        x ∉ _ /\ x' ∉ _ -> _ ‖ _;   (_ ∪ _ ‖ _) /\ _ -> nodup' _ _ ; """
		
		val lex = new BabyLexer(TOKENS)
		val p = new Parser(new Grammar(GRAMMAR), NOTATIONS)
		val tokens = lex.tokenize(program)
		println(tokens)
		for (prog <- p(tokens); t <- prog) println(t toPretty)
	}
  
	class BabyLexer(val patterns: List[(Regex, String)])
	{
	  /*
		def cat(token: String): String =
		{
		  patterns find { case (_, v) => v.unapplySeq(token).nonEmpty } match {
		    case Some((k, _)) => k
		    case _ => token  // individual category
		  }
		}
		*/
		
		def tokenize(text: String) = {
		  val l = ListBuffer.empty[Token]
		  var pos = 0
		  while (pos < text.length) {
  		  val (newPos, cat, value) = (
    		  for ((regex, cat) <- patterns) yield {
    		    val mo = regex.pattern.matcher(text).region(pos, text.length)
    		    if (mo.lookingAt())
    		      (mo.end(), cat, mo.group())
    		    else
    		      (0, null, null)
    		  }).maxBy((_._1))
    		if (newPos <= pos)
    		  throw new Exception(s"unrecognized token at '${text.substring(pos)}'")
    		if (cat != null) {
      		val tag = if (cat == "") value else cat
      		l.add(new Token(tag, value))
    		}
    		pos = newPos
		  }
		  l.toList
			//for (tok <- text.split("\\s+")) yield new Token(cat(tok), tok)
		} //toList
	}
	
	class Token(tag: String, value: String) extends Word(tag)
	{
		val text = value
		
		override def toString() =
			if (tag == text) tag else s"${tag}:${text}"
	}

  trait Annotation
  trait Annotated {
    this : Term =>
    val annot: List[Annotation] = List.empty
    
    def get[A <: Annotation : ClassTag] = annot collect { case a: A => a }
    def /+(a: Annotation) = _with(annot :+ a)
    def /++(as: Seq[Annotation]) = _with(annot ++ as)
    
    private def _with(l: List[Annotation]) =
      new Term(root, subtrees) with Annotated { override val annot = l }
  }
  implicit def annotate(t: Term) = t match {
    case a: Annotated => a
    case _ =>
      new Term(t.root, t.subtrees) with Annotated
  }
  
  case class DeductionHints(options: List[String]) extends Annotation
  
}


class Parser(grammar: Grammar, notations: Map[String, List[Term] => Term]) {

  import Parser._
  
  def this() = this(new Grammar(Parser.GRAMMAR), Parser.NOTATIONS)
  
	def apply(program: String): List[List[Term]] = {
		val lex = new BabyLexer(List(raw"\S+".r -> "", raw"\s+".r -> null))
		val tokens = lex.tokenize(program)
		apply(tokens)
	}

  def apply(tokens: List[Token]) = {
		val e = new EarleyParser(grammar);
		e.parseSentence(new SimpleSentence(tokens)).toList map toTree map toTerms
	}
  
  def toTree(pt: ontopt.pen.Tree): Tree[Word] = new Tree(pt.root, pt.subtrees map toTree toList)
  
  val E = raw"E\d+".r
 
  def toTerms(t: Tree[Word]): List[Term] = t.root.tag match {
    case "P" => t.subtrees flatMap toTerms
    case "S" | "E" | E() => List(collapseAnnotations(t.subtrees flatMap toTerms))
    case "#" => List(TI(Integer.parseInt(t.root.asInstanceOf[Token].text)))
    case "§" => List(TV(t.root.asInstanceOf[Token].text))
    case "=" | ";" => List()    
    case "[...]" => 
      List(TV("[...]") /+ DeductionHints(List(t.root.asInstanceOf[Token].text)))
    case k => 
      List(NOTATIONS(k)(t.subtrees filter (!_.isLeaf) flatMap toTerms toList)) // kind of assumes each subtree only yields one term
  }
  
  def collapseAnnotations(terms: Seq[Term]) = terms match {
    case head :: tail => 
      (head /: tail)((h, t) => h /++ t.annot)
    /* should never be called with an empty list */
  }

}
