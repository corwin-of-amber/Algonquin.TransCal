package language

import com.typesafe.scalalogging.LazyLogging
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
    TREE(I(x.toInt))
  }

  def identifier: Parser[Identifier] = Language.identifierRegex ^^ { x =>
    I(x)
  }

  def consts: Parser[Term] = seqToOrParser(builtinConsts) ^^ {
    case "⟨⟩" => TREE(Language.nilId)
    case "true" => TREE(Language.trueId)
    case "⊤" => TREE(Language.trueId)
    case "false" => TREE(Language.falseId)
    case "⊥" => TREE(Language.falseId)
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
      applied.tail match {
        case t: List[Term] if t.head.root == Language.tupleId && t.size == 1 => TREE(applied.head.root, applied.head.subtrees ++ applied.last.subtrees)
        case _ => TREE(applied.head.root, applied.head.subtrees ++ applied.drop(1))
      }
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

  def statementCommand: Parser[Term] = (expression ~ Language.tacticLiteral ~ expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement expr - $x")
    x match {
      case left ~ dir ~ right ~ anno =>
        val definitionTerm = new Tree(Language.tacticId, List(left, right))
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statementDefinition: Parser[Term] = (expression ~ seqToOrParser(builtinDefinitions) ~ expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement let - $x")
    x match {
      case left ~ dir ~ right ~ anno =>
        // This means lhs is a function
        val definitionTerm = right.root match {
          case Language.lambdaId =>
            val rightParams = if (right.subtrees(0).root == Language.tupleId) right.subtrees(0).subtrees else List(right.subtrees(0))
            val newRight = right.subtrees(1)
            val newLeft = TREE(left.root, left.subtrees ++ rightParams)
            TREE(I(dir), List(newLeft, newRight))
          case _ => TREE(I(dir), List(left, right))
        }
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statement: Parser[Term] = (expression ~ trueCondBuilderLiteral).? ~ (statementDefinition | statementCommand) ^^ { t =>
    logger.debug(s"statement - $t")
    def applyClojure(env: Seq[Term], t: Term): Term = {
      assert(env.forall(_.subtrees.isEmpty))
      t.root match {
        case Language.letId | Language.directedLetId =>
          val newParams = t.subtrees(0).leaves.filter(_.root.literal.toString.startsWith("?"))
          assert(env.intersect(newParams).isEmpty)
          TREE(t.root, List(t.subtrees(0), applyClojure(newParams ++ env, t.subtrees(1))))
        case Language.lambdaId =>
          val newParams = t.subtrees(0).leaves.filter(_.root.literal.toString.startsWith("?")).toList
          assert(env.intersect(newParams).isEmpty)

          val toAddParams = t.subtrees(1).leaves.map(i =>
            new Identifier("?" + i.root.literal.toString, i.root.kind, i.root.ns)
          ).map(i => TREE(i)).filter(env contains _).toList
          val paramsTree = {
            if (toAddParams.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, toAddParams ++ t.subtrees(0).subtrees)
              case i: Identifier => TREE(Language.tupleId, toAddParams :+ t.subtrees(0))
            }
          }

          val updatedLambda = TREE(Language.lambdaId, List(paramsTree, t.subtrees(1)))
          if (toAddParams.isEmpty) updatedLambda
          else TREE(Language.applyId,  updatedLambda +: env.toList)
        case i: Identifier => TREE(i, t.subtrees map (s => applyClojure(env, s)))
      }
    }
    val res = applyClojure(Seq.empty, t._2)
    t._1.map(x => TREE(trueCondBuilderId, List(x._1, res))).getOrElse(res)
  }

  def commands: Parser[Term] = seqToOrParser(Language.builtinCommands) ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(Language.commandId, List(TREE(new Identifier(x))))
  }

  def program: Parser[Term] = phrase((semicolonLiteral | "\n").* ~ (statement | commands) ~ rep((semicolonLiteral | "\n").+ ~ (statement | commands).?)) ^^ {
    case empty ~ sc ~ scCommaList => scCommaList.filter(_._2.nonEmpty).map(_._2.get).foldLeft(sc)((t1, t2) => new Tree(semicolonId, List(t1, t2)))
  }

  /** The known left operators at the moment */
  override def lefters: Map[Int, Set[String]] = Map(
    (Infixer.MIDDLE - 1, builtinSetArithOps.toSet),
    (Infixer.MIDDLE, Set(":+")),
    (Infixer.MIDDLE + 1, builtinBooleanOps.toSet),
    (Infixer.MIDDLE + 2, builtinAndOps.toSet),
    (Infixer.MIDDLE + 3, builtinOrOps.toSet),
    (Infixer.MIDDLE + 4, builtinIFFOps.toSet),
    (Infixer.MIDDLE + 5, builtinHighLevel.toSet)
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