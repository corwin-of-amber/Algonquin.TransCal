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

  def identifier: Parser[Identifier] = Language.identifierRegex ^^ { x =>
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

  def tuple: Parser[Term] = ("(,)" | ("(" ~ (exprInfixOperator ~ ",").+ ~ exprInfixOperator.? ~ ")")) ^^ { x =>
    val subtrees = x match {
      case "(" ~ (others: List[TranscalParser.this.~[Term, String]]) ~ (one: Option[Term]) ~ ")" => one.map(others.map(t => t._1) :+ _) getOrElse others.map(t => t._1)
      case "(,)" => List.empty
    }
    TREE(Language.tupleId, subtrees)
  }

  def exprValuesAndParens: Parser[Term] = (tuple | ("(" ~ exprInfixOperator ~ ")") | ("{" ~ exprInfixOperator ~ "}") | identifier | numeral | consts) ^^ { x =>
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

  def exprNot: Parser[Term] = (rep(seqToOrParser(builtinNotOps)) ~ exprApply) ^^ { x =>
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

  def expression: Parser[Term] = exprInfixOperator ^^ { x: Term =>
    replaceUnicode(x)
  }

  def annotation: Parser[Term] = ("[" ~ "[^\\]]+".r ~ "]") ^^ {
    case _ ~ anno ~ _ => TREE(I(anno))
  }

  def statementCommand: Parser[Term] = (expression ~ Language.commandLiteral ~ expression ~ annotation.?) ^^ { x =>
    logger.debug(s"statement expr - $x")
    x match {
      case left ~ dir ~ right ~ anno =>
        val definitionTerm = new Tree(Language.commandId, List(left, right))
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statementDefinition: Parser[Term] = (expression ~ seqToOrParser(builtinDefinitions) ~ expression ~ annotation.?) ^^ { x =>
    logger.debug(s"statement let - $x")
    x match {
      case left ~ dir ~ right ~ anno =>
        // This means lhs is a function
        val definitionTerm = right.root match {
        case Language.lambdaId =>
          val rightParams = if (right.subtrees(0).root == Language.tupleId) right.subtrees(0).subtrees else List(right.subtrees(0))
          val newRight = right.subtrees(1)
          val leftChildren = if (left.root == Language.applyId) left.subtrees else List(left)
          val newLeft = TREE(Language.applyId, leftChildren ++ rightParams)
          TREE(I(dir), List(newLeft, newRight))
        case _ => TREE(I(dir), List(left, right))
        }
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statement: Parser[Term] = statementDefinition | statementCommand ^^ { t =>
      def expandParams(env: Set[Identifier], t: Term): Term = {
        t.root match {
          case Language.lambdaId =>
            val params = t.subtrees(0).leaves.filterNot(_.root.literal.toString.startsWith("?")).toList
            assert(env.intersect(params.map(_.root).toSet).isEmpty)
            val paramsTree = t.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, params ++ t.subtrees(0).subtrees)
              case i: Identifier => TREE(Language.tupleId, params :+ t.subtrees(0))
            }
            TREE(Language.applyId, TREE(Language.lambdaId, List(paramsTree, t.subtrees(1))) :: params)
          case i: Identifier => TREE(i, t.subtrees map (s => expandParams(env, s)))
        }
      }

      expandParams(Set.empty, t)
  }

  def commands: Parser[Term] = seqToOrParser(Language.builtinCommands) ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(new Identifier("Command", x))
  }

  def program: Parser[Term] = phrase((";" | "\n").* ~ (statement | commands) ~ rep((";" | "\n").+ ~ (statement | commands).?)) ^^ {
    case empty ~ sc ~ scCommaList => scCommaList.filter(_._2.nonEmpty).map(_._2.get).foldLeft(sc)((t1, t2) => new Tree(I(";"), List(t1, t2)))
  }

  /** The known left operators at the moment */
  override def lefters: Map[Int, Set[String]] = Map(
    (Infixer.MIDDLE - 1, builtinSetArithOps.toSet),
    (Infixer.MIDDLE, Set(":+")),
    (Infixer.MIDDLE + 1, builtinBooleanOps.toSet),
    (Infixer.MIDDLE + 2, builtinAndOps.toSet),
    (Infixer.MIDDLE + 3, builtinOrOps.toSet),
    (Infixer.MIDDLE + 4, builtinIFFOps.toSet),
    (Infixer.HIGH, builtinHighLevel.toSet)
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