package synthesis.language

import com.typesafe.scalalogging.LazyLogging
import relentless.BasicSignature
import syntax.AstSugar._
import syntax.{Identifier, Tree}

import scala.util.parsing.combinator.RegexParsers


class TranscalParser extends RegexParsers with LazyLogging with Parser[Term] {
  def apply(programText: String): Term = parse(program, programText) match {
    case Success(matched, text) => matched
    case Failure(msg, text) =>
      throw new RuntimeException(s"FAILURE: $msg \n $text")
    case Error(msg, text) =>
      throw new RuntimeException(s"ERROR: $msg \n $text")
  }

  override val whiteSpace = """[ \t]+""".r

  private def TERM(x: Any) = TI(x)

  private def TREE(x: Identifier) = T(x)

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
    TERM(x.toInt)
  }

  def identifier: Parser[Identifier] = raw"[?]?[\w'_]+".r ^^ { x =>
    logger.trace(s"identifier - $x")
    I(x)
  }

  def consts: Parser[Term] = "⟨⟩|true|false".r ^^ { x =>
    logger.trace(s"const - $x")
    x match {
      case "⟨⟩" => BasicSignature._nil
      case "true" => BasicSignature.tt
      case "false" => BasicSignature.ff
    }
  }

  def exprValuesAndParens: Parser[Term] = (("(" ~ exprLambdaSplit ~ ")") | ("{" ~ exprLambdaSplit ~ "}") | identifier | numeral | consts) ^^ { x =>
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

  def exprNot: Parser[Term] = rep("(~|¬)".r) ~ exprApply ^^ { x =>
    if (x._1.nonEmpty) logger.trace(s"not - $x")
    x match {
      case applied ~ exp => applied.foldLeft(exp)((t, _) => ~DSL(t))
    }
  }

  def exprSetAndArith: Parser[Term] = exprNot ~ rep(exprNot ~ "(\\+\\+|\\+|-|∪)".r)  ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"set and arith - $x")
    x match {
      case exp ~ expOpList=>
        val ops = expOpList.map(_._2)
        val exps = exp :: expOpList.map(_._1)
        leftFolder(exps, ops)
    }
  }

  def exprListConstruct: Parser[Term] = exprSetAndArith ~ rep((":+" | "::") ~ exprSetAndArith) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"List construction - $x")
    val firstExp ~ csExpList = x
    def buildList(exps: List[Term], ops: List[String]): Term = (exps, ops) match {
      case (exp :: eRest, "::" :: oRest) => TERM("::", List(exp, buildList(eRest.asInstanceOf[List[Term]], oRest.asInstanceOf[List[String]])))
      case (exp1 :: exp2 :: eRest, ":+" :: oRest) =>
        buildList(TERM(":+", List(exp1, exp2)) :: eRest.asInstanceOf[List[Term]], oRest.asInstanceOf[List[String]])
      case (exp, Nil) =>
        exp.head
    }
    buildList(firstExp :: csExpList.map(_._2), csExpList.map(_._1))
  }

  def exprBooleanOp: Parser[Term] = exprListConstruct ~ rep("(=|≠|∈|∉|\\|\\||‖)".r ~ exprListConstruct) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"bool op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops)
    }
  }

  def exprAnd: Parser[Term] = exprBooleanOp ~ rep("(/\\\\|∧)".r ~ exprBooleanOp) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"and op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops)
    }
  }

  def exprOr: Parser[Term] = exprAnd ~ rep("(\\\\/|∨)".r ~ exprAnd) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"or op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops)
    }
  }

  def exprIFF: Parser[Term] = exprOr ~ rep("<->" ~ exprOr) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"iff op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        leftFolder(exps, ops)
    }
  }

  def exprDrags: Parser[Term] = (exprIFF ~ rep(("->"|"=>") ~ exprIFF)) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"drags op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        rightFolder(exps, ops)
    }
  }

  def exprLambdaSplit: Parser[Term] = (exprDrags ~ rep("[:/↦]".r ~ exprDrags)) ^^ { x =>
    if (x._2.nonEmpty) logger.debug(s"lambda split op - $x")
    x match {
      case exp ~ expOpList =>
        val ops = expOpList.map(_._1)
        val exps = exp :: expOpList.map(_._2)
        rightFolder(exps, ops)
    }
  }

  def expression: Parser[Term] = exprLambdaSplit

  def annotation: Parser[String] = "\\[.+?\\]".r ^^ {
    case anno => anno.tail.take(anno.length - 2)
  }

  def statement: Parser[Term] = (expression ~ annotation.?) ^^ { x =>
    logger.debug(s"statement - $x")
    x match {
      case expr ~ anno => anno.map(a => new Tree(new Identifier("Annotation", a), List(expr))) getOrElse expr
    }
  }

  def commands: Parser[Term] = "(->|→|<-|←|\\[\\]|□)".r ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(new Identifier("Command", x))
  }

  def program: Parser[Term] = phrase((statement | commands) ~ rep(rep1("(;|\\n)".r) ~ (statement | commands))) ^^ {
    case sc ~ scCommaList => scCommaList.map(_._2).foldLeft(sc)((t1, t2) => new Tree(I(";"), List(t1, t2)))
  }

  private def leftFolder(exps: List[Term], op: String): Term = leftFolder(exps, List.fill(exps.length - 1)(op))

  private def leftFolder(exps: List[Term], ops1: List[String]): Term = {
    val ops = ops1.map(TERM)
    assert(exps.nonEmpty && ops.size == exps.size - 1)
    // Use apply for each op from left to right (right should be highest in tree)
    ops.zip(exps.tail).foldLeft(exps.head)((exp, tup) => tup._1 :@ (exp, tup._2))
  }

  private def rightFolder(exps: List[Term], op: String): Term = rightFolder(exps, List.fill(exps.length - 1)(op))

  private def rightFolder(exps: List[Term], ops1: List[String]): Term = {
    val ops = ops1.map(TERM)
    assert(exps.nonEmpty && ops.size == exps.size - 1)
    ops.zip(exps.take(exps.length - 1)).foldRight(exps.last)((tup, exp) => tup._1 :@ (exp, tup._2))
  }
}

object TranscalParser {

}