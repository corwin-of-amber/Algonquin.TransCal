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


  
object Parser {

  val GRAMMAR = 
		raw"""P -> | P S
		      S -> E ;
          E -> E100
          E100     -> N: | E99
              N:   -> E99 : E100
          E99      -> N→ | E95
              N→  -> E95 -> E99
          E95      -> N↔︎ | E85
              N↔︎  -> E95 <-> E85
          E85      -> N∨ | E80
              N∨   -> E85 \/ E80
          E80      -> N∧ | E75
              N∧   -> E80 /\ E75
          E75      -> N¬ | E70
              N¬   -> ~ E75
          E70      -> N= | N≠ | E60
              N=   -> E70 = E60
              N≠   -> E70 ≠ E60
          E60      -> N:: | E50
              N::  -> E50 :: E60
          E50      -> N+ | N- | N‖ | E10
              N+   -> E50 + E10
              N-   -> E50 - E10
              N‖   -> E50 || E10
          E10      -> N@ | E0
              N@   -> E10 E0
          E0       -> N{} | # | § | ( E100 )
              N{}  -> { E100 }"""
	
	val TOKENS = List(raw"\d+".r -> "#",   // numeral
	                  raw"[?]?[\w']+".r -> "§",   // identifier
	                  "[(){}+-=≠~<>:]".r -> "",
	                  raw"\\/|/\\|\|\||->|<->|::".r -> "",
	                  raw"\s+".r -> null)

		
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
      "N::"  -> op(BasicSignature.cons _),
      "N@"   -> op(_ :@ _)
  )
  
	def main(args: Array[String])
	{
		val program = raw"x /\ y -> 1; 1 -> ?nodup' x y; xs = x :: xs';"
		
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
  
  def scrub(t: Tree[Word]): Tree[Word] = t.root.tag match {
    case E() => scrub(t.subtrees.head)
    case _ => new Tree(t.root, t.subtrees map scrub)
  }
	
  def toTerms(t: Tree[Word]): List[Term] = t.root.tag match {
    case "P" | "S" | "E" | E() => t.subtrees flatMap toTerms
    case "#" => List(TI(t.root.asInstanceOf[Token].text))
    case "§" => List(TV(t.root.asInstanceOf[Token].text))
    case "=" | ";" => List()    
    case k => 
      List(NOTATIONS(k)(t.subtrees filter (!_.isLeaf) flatMap toTerms toList)) // kind of assumes each subtree only yields one term
  }

}
