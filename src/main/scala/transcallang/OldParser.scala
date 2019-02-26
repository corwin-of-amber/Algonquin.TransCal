package transcallang

import java.io.{BufferedReader, Reader}

import com.typesafe.scalalogging.LazyLogging
import ontopt.pen.{EarleyParser, Grammar, SimpleSentence, Word}
import relentless.BasicSignature
import syntax.AstSugar.{TI, TV, Term}
import syntax.{Identifier, Tree}
import syntax.AstSugar._

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.util.matching.Regex

object OldParser {
  val GRAMMAR =
    raw"""P -> | S | C | P ; S | P ; C | P ;
		      S -> E
          S -> E [...]
          C -> C→ | C← | C□
              C→   -> -> | →
              C←   -> <- | ←
              C□   -> [] | □
          E -> E100
          E100     -> N: | N/ | N↦ | E99
              N:   -> E99 : E100
              N/   -> E99 / E100
              N↦   -> E99 ↦ E100
          E99      -> N→ | N⇒ | E95
              N→  -> E95 -> E99
              N⇒  -> E95 => E99
          E95      -> N↔︎ | E85
              N↔︎  -> E95 <-> E85
          E85      -> N∨ | E80
              N∨   -> E85 \/ E80 | E85 ∨ E80
          E80      -> N∧ | E70
              N∧   -> E80 /\ E70 | E80 ∧ E70
          E70      -> N= | N≠ | N< | N> | N∈ | N∉ | N‖ | E60
              N=   -> E70 = E60
              N≠   -> E70 ≠ E60 | E70 != E60
              N<   -> E70 < E60
              N>   -> E70 > E60
              N≤   -> E70 ≤ E60 | E70 <= E60
              N≥   -> E70 ≥ E60 | E70 >= E60
              N∈   -> E70 ∈ E60
              N∉   -> E70 ∉ E60
              N‖   -> E70 || E60 | E70 ‖ E60
          E60      -> N:: | N:+ | E50
              N::  -> E50 :: E60
              N:+  -> E60 :+ E50
          E50      -> N+ | N- | N∪ | N++ | E15
              N+   -> E50 + E15
              N-   -> E50 - E15
              N∪   -> E50 ∪ E15
              N++  -> E50 ++ E15
          E15      -> N¬ | E10
              N¬   -> ~ E15 | ¬ E15
          E10      -> N@ | E0
              N@   -> E10 E0
          E0       -> N{} | N() | N⟨⟩ | N⊤ | N⊥ | # | §
              N()  -> ( E100 )
              N{}  -> { E100 }
              N⟨⟩   -> ⟨ ⟩
              N⊤   -> ⊤
              N⊥   -> ⊥
          VARS     -> § | VARS §
"""

  val TOKENS = List(raw"\d+".r -> "#",              // numeral
    raw"[?]?[\w'_1⃝]+".r -> "§",    // identifier
    raw"\d⃝".r -> "§",              // anchor name (circled numeral)
    raw"\(\d+\)".r -> "§",
    raw"\[.+?\]".r -> "[...]",      // hints
    "[@(){}+-=≠~<>:∈∉∪‖⟨⟩↦⊤⊥]".r -> "",
    raw"\\/|/\\|\|\||<-|->|<->|=>|<=|>=|!=|\[\]|::|\+\+|:\+".r -> "",
    raw"w/o|\\".r -> "",
    raw"/\*[\s\S]*?\*/".r -> null,
    raw"\s+".r -> null)


  def op(op: => Term): List[Term] => Term = (_: List[Term]) => op
  def op(op: Term => Term): List[Term] => Term = (l: List[Term]) => op(l.head)
  def op(op: (Term, Term) => Term): List[Term] => Term = (l: List[Term]) => op(l.head, l(1))

  val NOTATIONS: Map[String, List[Term] => Term] = Map(
    "N:"   -> op(_ :- _),
    "N→"   -> op(_ -> _),
    "N⇒"   -> op(BasicSignature.`⇒:` _),
    "N↔︎"   -> op(_ <-> _),
    "N∧"   -> op(_ & _),
    "N∨"   -> op(_ | _),
    "N¬"   -> op(~_),
    "N="   -> op(_ =:= _),
    "N/"   -> op(_ /: _),
    "N↦"   -> op(_ ↦ _),
    "N≠"   -> op(BasicSignature.!=:= _),
    "N<"   -> op(BasicSignature.< _),
    "N>"   -> op(BasicSignature.> _),
    "N≤"   -> op(BasicSignature.≤ _),
    "N≥"   -> op(BasicSignature.≥ _),
    "N::"  -> op(BasicSignature.cons _),
    "N:+"  -> op(BasicSignature.snoc _),
    "N‖"   -> op(BasicSignature.set_disj _),
    "N∈"   -> op(BasicSignature.in _),
    "N∉"   -> op(BasicSignature.not_in _),
    "N∪"   -> op(BasicSignature.set_union _),
    "N++"  -> op(BasicSignature.++ _),
    "N@"   -> op(_ :@ _),
    "N()"  -> op(x => x),
    "N{}"  -> op(BasicSignature.`{}` _),
    "N⟨⟩"   -> op(BasicSignature._nil),
    "N⊤"   -> op(BasicSignature.tt),
    "N⊥"   -> op(BasicSignature.ff),
    "N@[]" -> TI("@[]").apply,

    /* command notations */
    "C→"   -> op(TI("→")),
    "C←"  -> op(TI("←")),
    "C□"   -> op(TI("□"))
  )

  // -----------
  // Blocks Part
  // -----------

  def getLines(f: BufferedReader): Stream[String] = {
    val line = f.readLine()
    if (line == null) Stream.empty else line #:: getLines(f)
  }

  def splitBlocks(s: Stream[String]): Stream[String] = s match {
    case Stream.Empty => Stream.empty
    case firstH #:: rest => rest.span(_.matches(raw"\s.*")) match {
      case (firstT, rest2) =>
        def splitRest = splitBlocks(rest2 dropWhile (_ == ""))
        (firstH #:: firstT).mkString("\n") #:: splitRest
    }
  }

  def getBlocks(f: BufferedReader): Stream[String] = splitBlocks(getLines(f))
  def getBlocks(f: Reader): Stream[String] = getBlocks(new BufferedReader(f))

  class BabyLexer(val patterns: List[(Regex, String)]) {
    def tokenize(text: String): List[Token] = {
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
          }).maxBy(_._1)
        if (newPos <= pos)
          throw new Exception(s"unrecognized token at '${text.substring(pos)}'")
        if (cat != null) {
          val tag = if (cat == "") value else cat
          l.add(new Token(tag, value))
        }
        pos = newPos
      }
      l.toList
    }
  }

  class Token(tag: String, value: String) extends Word(tag)
  {
    val text: String = value

    override def toString: String =
      if (tag == text) tag else s"$tag:$text"
  }

  trait Annotation
  trait Annotated[X] {
    this: X =>
    val annot: List[Annotation] = List.empty

    def get[A <: Annotation : ClassTag]: List[A] = annot collect { case a: A => a }
    def /+(a: Annotation)(implicit wax: WithAnnotation[X]): X with Annotated[X] = _with(annot :+ a)
    def /++(as: Seq[Annotation])(implicit wax: WithAnnotation[X]): X with Annotated[X] = _with(annot ++ as)

    protected def _with(l: List[Annotation])(implicit wax: WithAnnotation[X]): X with Annotated[X] = wax.copyWith(this, l)
  }

  trait WithAnnotation[X] {
    def copyWith(x: X, l: List[Annotation]): X with Annotated[X]
  }

  implicit object WithAnnotationForTerm extends WithAnnotation[Term] {
    def copyWith(t: Term, l: List[Annotation]): Term with Annotated[Term] =
      new Term(t.root, t.subtrees) with Annotated[Term] { override val annot: List[Annotation] = l }
  }

  implicit def annotate(t: Term): Term with Annotated[Term] = t match {
    case a: Annotated[Term] @unchecked /* it cannot really be anything else because of this: X in Annotated[X] */ => a
    case _ => implicitly[WithAnnotation[Term]].copyWith(t, List.empty)
  }

  case class DeductionHints(options: List[String]) extends Annotation
}


class OldParser(grammar: Grammar) extends Parser[Term] with LazyLogging {

  import OldParser._

  def this() = this(new Grammar(OldParser.GRAMMAR))

  private lazy val earley = new EarleyParser(grammar)
  private val E: Regex = raw"E\d+".r
  private val BRACKETS: Regex = raw"\[(.*)\]".r

  def apply(program: String): Term = {
    val lex = new BabyLexer(TOKENS)
    val tokens = lex.tokenize(program.split(raw"/\s*\n").mkString("/ ").split("\n").filter(_.stripLineEnd != "").mkString(";"))
    apply(tokens)
  }

  private def apply(tokens: List[Token]): Term = {
    val parsed = earley.parseSentence(new SimpleSentence(tokens))
    if (parsed.size() > 1) logger.warn("Ambiguous program")
    val terms: List[Term] = toTerms(toTree(parsed.head))

    if (terms.size == 1) {
      terms.head
    } else {
      val res = terms.take(terms.size - 1).foldRight(terms.last)((term: Term, current: Term) => new Tree[Identifier](I(";"), List[Term](term, current)))
      res
    }
  }

  private def error: String = {
    val pm = earley.diagnoseError()
    s"at '${pm.token}': expecting ${pm.expecting mkString " "}"
  }

  private def toTree(pt: ontopt.pen.Tree): Tree[Word] = new Tree(pt.root, pt.subtrees.map(toTree).toList)

  private def toTerms(t: Tree[Word]): List[Term] = t.root.tag match {
    case "P" | "C" | "VARS" => t.subtrees flatMap toTerms
    //case "C" => List(TI(t.subtrees(0).root.asInstanceOf[Token].text))
    case "S" | "E" | E() => List(collapseAnnotations(t.subtrees flatMap toTerms))
    case "#" => List(TI(Integer.parseInt(t.root.asInstanceOf[Token].text)))
    case "§" => List(TV(t.root.asInstanceOf[Token].text))
    case "=" | ";" => List()
    case "[...]" =>
      List(TV("[...]") /+ DeductionHints(parseOptions(t.root.asInstanceOf[Token].text)))
    case k =>
      List(NOTATIONS(k)(t.subtrees filter (!_.isLeaf) flatMap toTerms)) // kind of assumes each subtree only yields one term
  }

  private def parseOptions(s: String): List[String] = (s match {
    case BRACKETS(s2) => s2
    case s1 => s1
  }).split(raw"\s+").toList

  private def collapseAnnotations(terms: Seq[Term]): Term = terms match {
    case head :: tail =>
      (head /: tail) ((h, t) => h /++ t.annot)
    /* should never be called with an empty list */
  }
}
