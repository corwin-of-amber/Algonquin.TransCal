package language

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature
import syntax.AstSugar._
import syntax.{Identifier, Tree}
import Language._

import scala.util.parsing.combinator.RegexParsers


class TranscalParser extends RegexParsers with LazyLogging with Parser[Term] with TermInfixer {
  def apply(programText: String): Term = {
    // Clean comments and new lines inside parenthesis
    // TODO: replace comments with whitespace for receiving errorl location correctly
    def cleanLineComments(text: String): String = "(.*?)(//.+)?".r.replaceAllIn(text, m => m.group(1))

    def cleanMultilineComments(text: String): String = "/\\*(.|\n)*?\\*/".r.replaceAllIn(text, "")

    val text = cleanMultilineComments(programText).split("\n").map(cleanLineComments).mkString("\n")
    parse(program, text) match {
      case Success(matched, text) => matched
      case Failure(msg, text) =>
        throw new RuntimeException(s"FAILURE: $msg in position ${text.pos} \n ${text.source}")
      case Error(msg, text) =>
        throw new RuntimeException(s"ERROR: $msg in position ${text.pos} \n ${text.source}")
    }
  }

  override val whiteSpace = """[ \t]+""".r

  private def TREE(x: Identifier, subtrees: List[Tree[Identifier]] = List.empty): Term = T(x, subtrees)

  // Example of defining a parser for a word
  // def word: Parser[String]    = """[a-z]+""".r ^^ { _.toString }

  // A little explanation for future students of Shachar
  // This system uses combinators to create the EBNF.
  // ~ means one after another.
  // | is options
  // ^^ we finished our regex/parser now what we do with it
  // You can google the rest.

  def numeral: Parser[Term] = "\\d+".r ^^ { x =>
    logger.trace(s"numeral - $x")
    TREE(I(x.toInt))
  }

  def identifier: Parser[Identifier] = "[?]?[\\w'_]+".r ^^ { x =>
    logger.trace(s"identifier - $x")
    I(x)
  }

  def consts: Parser[Term] = seqToOrParser(builtinConsts) ^^ { x =>
    logger.trace(s"const - $x")
    x match {
      case "⟨⟩" => BasicSignature._nil
      case "true" => BasicSignature.tt
      case "⊤" => BasicSignature.tt
      case "false" => BasicSignature.ff
      case "⊥" => BasicSignature.ff
    }
  }

  def exprValuesAndParens: Parser[Term] = (("(" ~ exprDrags ~ ")") | ("{" ~ exprDrags ~ "}") | identifier | numeral | consts) ^^ { x =>
    if (x.isInstanceOf[TranscalParser.this.~[Any, Any]]) logger.trace(s"value or parens - $x")
    x match {
      case m: Term => m
      case i: Identifier => TREE(i)
      case _ ~ t ~ _ => t.asInstanceOf[Term]
    }
  }

  def exprApply: Parser[Term] = rep1(exprValuesAndParens) ^^ { applied =>
    if (applied.tail.isEmpty) applied.head
    else {
      logger.trace(s"apply - $applied")
      TREE(I("@"), applied)
    }
  }

  def exprNot: Parser[Term] = rep(seqToOrParser(builtinNotOps)) ~ exprApply ^^ { x =>
    if (x._1.nonEmpty) logger.trace(s"not - $x")
    x match {
      case applied ~ exp => applied.foldLeft(exp)((t, _) => ~DSL(t))
    }
  }

  def exprInfixOperator: Parser[Term] = operatorsParser(exprNot)

  private val normalToUnicode: Map[String, String] = Map("\\/" -> "∨", "/\\" -> "∧", "!=" -> "≠", "||" -> "‖", "<=" -> "≤", ">=" -> "≥", "=>" -> "⇒")

  private def translateUnicode(t: String): String = normalToUnicode.getOrElse(t, t)

  private def replaceUnicode(t: Term): Term = t match {
    case t: Term if t.isLeaf => t
    case t: Term => new Tree[Identifier](new Identifier(translateUnicode(t.root.literal.toString), t.root.kind, t.root.ns), t.subtrees.map(replaceUnicode))
  }

  def exprDrags: Parser[Term] = (exprInfixOperator ~ rep(seqToOrParser(builtinDragOps) ~ exprInfixOperator)) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"drags op - $x")

    def merge(exps: List[Term], ops: List[String]): Term = {
      if (exps.length == 1) exps.head
      else {
        new Tree(I(ops.head), List(exps.head, merge(exps.tail, ops.tail)))
      }
    }

    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1).map(translateUnicode)
        val exps = exp :: expOpList.map(_._2)
        merge(exps, ops)
    }
  }

  def expression: Parser[Term] = exprDrags ^^ { x: Term =>
    replaceUnicode(x)
  }

  def annotation: Parser[Term] = "[" ~ "[^\\]]+".r ~ "]" ^^ {
    case _ ~ anno ~ _ => TREE(I(anno))
  }

  def statement: Parser[Term] = (expression ~ annotation.?) ^^ { x =>
    logger.debug(s"statement - $x")
    x match {
      case expr ~ anno => anno.map(a => new Tree(new Identifier("Annotation"), List(expr, a))) getOrElse expr
    }
  }

  def commands: Parser[Term] = seqToOrParser(Language.builtinCommands) ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(new Identifier("Command", x))
  }

  def program: Parser[Term] = phrase((";" | "\n").* ~ (statement | commands) ~ rep((";" | "\n").+ ~ (statement | commands).?)) ^^ {
    case empty ~ sc ~ scCommaList => scCommaList.filter(_._2.nonEmpty).map(_._2.get).foldLeft(sc)((t1, t2) => new Tree(I(";"), List(t1, t2)))
  }

  private def leftFolder(exps: List[Term], op: String): Term = leftFolder(exps, List.fill(exps.length - 1)(op))

  private def leftFolder(exps: List[Term], ops: List[String]): Term = {
    if (ops.isEmpty) exps.head
    else {
      assert(exps.nonEmpty && ops.size == exps.size - 1)
      TREE(I("@"), List(TREE(I(ops.head)), exps.head, leftFolder(exps.tail, ops.tail)))
    }
  }

  private def rightFolder(exps: List[Term], op: String): Term = rightFolder(exps, List.fill(exps.length - 1)(op))

  private def rightFolder(exps: List[Term], ops1: List[String]): Term = {
    val ops = ops1.map(x => TREE(I(x)))
    assert(exps.nonEmpty && ops.size == exps.size - 1)
    ops.zip(exps.take(exps.length - 1)).foldRight(exps.last)((tup, exp) => tup._1 :@ (exp, tup._2))
  }

  /** The known left operators at the moment */
  override def lefters: Map[Int, Set[String]] = Map(
    (Infixer.MIDDLE - 1, builtinSetArithOps.toSet),
    (Infixer.MIDDLE, Set(":+")),
    (Infixer.MIDDLE + 1, builtinBooleanOps.toSet),
    (Infixer.MIDDLE + 2, builtinAndOps.toSet),
    (Infixer.MIDDLE + 3, builtinOrOps.toSet),
    (Infixer.MIDDLE + 4, builtinIFFOps.toSet)
  )

  /** The known right operators at the moment */
  override def righters: Map[Int, Set[String]] = Map(
    (Infixer.MIDDLE, Set("::"))
  )

  /** A way to rebuild the the class */
  override def build(lefters: Map[Int, Set[String]], righters: Map[Int, Set[String]]): TermInfixer =
    throw new NotImplementedError()

  private def seqToOrParser(seq: Seq[String]): Parser[String] = seq.map(literal).reduce((p1, p2) => p1 | p2)
}