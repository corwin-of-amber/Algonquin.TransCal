package synthesis.language

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature
import syntax.AstSugar._
import syntax.{Identifier, Tree}

import scala.util.parsing.combinator.RegexParsers


class TranscalParser extends RegexParsers with LazyLogging with Parser[Term] {
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

  def consts: Parser[Term] = ("⟨⟩"|"true"|"false"|"⊤"|"⊥") ^^ { x =>
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
    if (applied.size > 1) logger.trace(s"apply - $applied")
    if (applied.tail.isEmpty) applied.head
    else applied.head :@ applied.tail
  }

  def exprNot: Parser[Term] = rep(seqToOrParser(builtinNotOps)) ~ exprApply ^^ { x =>
    if (x._1.nonEmpty) logger.trace(s"not - $x")
    x match {
      case applied ~ exp => applied.foldLeft(exp)((t, _) => ~DSL(t))
    }
  }

  def exprSetAndArith: Parser[Term] = exprNot ~ rep(seqToOrParser(builtinSetArithOps) ~ exprNot)  ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"set and arith - $x")
    x match {
      case exp ~ expOpList=>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops)
    }
  }

  def exprListConstruct: Parser[Term] = exprSetAndArith ~ rep(seqToOrParser(builtinSetBuildingOps) ~ exprSetAndArith) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"List construction - $x")
    val firstExp ~ csExpList = x
    def buildList(exps: List[Term], ops: List[String]): Term = (exps, ops) match {
      case (exp :: eRest, "::" :: oRest) => TREE(I("::"), List(exp, buildList(eRest.asInstanceOf[List[Term]], oRest.asInstanceOf[List[String]])))
      case (exp1 :: exp2 :: eRest, ":+" :: oRest) =>
        buildList(TREE(I(":+"), List(exp1, exp2)) :: eRest.asInstanceOf[List[Term]], oRest.asInstanceOf[List[String]])
      case (exp, Nil) =>
        exp.head
    }
    buildList(firstExp :: csExpList.map(_._2), csExpList.map(_._1))
  }

  private val normalToUnicode: Map[String, String] = Map("\\/" -> "∨", "/\\" -> "∧", "!=" -> "≠", "||" -> "‖", "<=" ->"≤", ">=" -> "≥", "=>" -> "⇒")
  private def translateUnicode(t: String): String = normalToUnicode.getOrElse(t, t)

  def exprBooleanOp: Parser[Term] = exprListConstruct ~ rep(seqToOrParser(builtinBooleanOps) ~ exprListConstruct) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"bool op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops.map(translateUnicode))
    }
  }

  def exprDrags: Parser[Term] = (exprBooleanOp ~ rep(seqToOrParser(builtinDragOps) ~ exprBooleanOp)) ^^ { x =>
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

  def expression: Parser[Term] = exprDrags

  def annotation: Parser[Term] = "[" ~ "[^\\]]+".r ~ "]" ^^ {
    case _ ~ anno ~ _ => TREE(I(anno))
  }

  def statement: Parser[Term] = (expression ~ annotation.?) ^^ { x =>
    logger.debug(s"statement - $x")
    x match {
      case expr ~ anno => anno.map(a => new Tree(new Identifier("Annotation"), List(expr, a))) getOrElse expr
    }
  }

  def commands: Parser[Term] = seqToOrParser(builtinCommands) ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(new Identifier("Command", x))
  }

  def program: Parser[Term] = phrase((";"|"\n").* ~ (statement | commands) ~ rep((";"|"\n").+ ~ (statement | commands).?)) ^^ {
    case empty ~ sc ~ scCommaList => scCommaList.filter(_._2.nonEmpty).map(_._2.get).foldLeft(sc)((t1, t2) => new Tree(I(";"), List(t1, t2)))
  }

  private def leftFolder(exps: List[Term], op: String): Term = leftFolder(exps, List.fill(exps.length - 1)(op))

  private def leftFolder(exps: List[Term], ops1: List[String]): Term = {
    val ops = ops1.map(x => TREE(I(x)))
    assert(exps.nonEmpty && ops.size == exps.size - 1)
    // Use apply for each op from left to right (right should be highest in tree)
    ops.zip(exps.tail).foldLeft(exps.head)((exp, tup) => tup._1 :@ (exp, tup._2))
  }

  private def rightFolder(exps: List[Term], op: String): Term = rightFolder(exps, List.fill(exps.length - 1)(op))

  private def rightFolder(exps: List[Term], ops1: List[String]): Term = {
    val ops = ops1.map(x => TREE(I(x)))
    assert(exps.nonEmpty && ops.size == exps.size - 1)
    ops.zip(exps.take(exps.length - 1)).foldRight(exps.last)((tup, exp) => tup._1 :@ (exp, tup._2))
  }

  private def seqToOrParser(seq: Seq[String]): Parser[String] = seq.map(literal).reduce((p1, p2) => p1 | p2)
}