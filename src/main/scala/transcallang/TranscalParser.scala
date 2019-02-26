package transcallang

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

  def polymorphicTypes: Parser[Term] = Language.typeRegex ~ ('[' ~ types ~ ']').? ^^ {
    case x ~ None => TREE(I(x))
    case x ~ Some('[' ~ polymorphic ~ ']') => TREE(Language.innerTypeId, List(TREE(I(x)), polymorphic))
  }

  def types: Parser[Term] = polymorphicTypes ~ (Language.mapTypeLiteral ~ types).? ^^ {
    case x ~ None => TREE(I(x))
    case x ~ Some(Language.mapTypeLiteral ~ recursive) => TREE(Language.mapTypeId, List(TREE(I(x)), recursive))
  }

  def identifier: Parser[Term] = Language.identifierRegex ~ (Language.typeBuilderLiteral ~ types).? ^^ {
    case x ~ None => TREE(I(x))
    case x ~ Some(Language.typeBuilderLiteral ~ z) => TREE(Language.typeBuilderId, List(TREE(I(x)), z))
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
      case "{" ~ t ~ "}" => TREE(Language.setId, List(t.asInstanceOf[Term]))
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

  def statementDefinition: Parser[Term] = ((expression ~ trueCondBuilderLiteral).? ~ expression ~ seqToOrParser(builtinDefinitions) ~ expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement let - $x")
    x match {
      case op ~ left ~ dir ~ right ~ anno =>
        // This means lhs is a function
        val (fixedLeft, fixedRight) = right.root match {
          case Language.lambdaId =>
            val rightParams = if (right.subtrees(0).root == Language.tupleId) right.subtrees(0).subtrees else List(right.subtrees(0))
            val newRight = right.subtrees(1)
            val newLeft = TREE(left.root, left.subtrees ++ rightParams)
            (newLeft, newRight)
          case _ => (left, right)
        }
        val conditionedLeft = op.map(o => TREE(trueCondBuilderId, List(o._1, fixedLeft))).getOrElse(fixedLeft)
        val definitionTerm = TREE(I(dir), List(conditionedLeft, fixedRight))
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statement: Parser[Term] = (statementDefinition | statementCommand) ^^ { t =>
    logger.debug(s"statement - ${t}")

    def applyClojure(env: Seq[Term], t: Term): Term = {
      assert(env.forall(_.subtrees.isEmpty))
      val replacemnetVarPrefix = "?autovar"
      t.root match {
        case Language.letId | Language.directedLetId =>
          val newParams = t.subtrees(0).leaves.filter(_.root.literal.toString.startsWith("?"))
          assert(env.intersect(newParams).isEmpty)
          TREE(t.root, List(t.subtrees(0), applyClojure(newParams ++ env, t.subtrees(1))))
        case Language.lambdaId =>
          val newParams = t.subtrees(0).leaves.filter(_.root.literal.toString.startsWith("?")).toList
          assert(env.intersect(newParams).isEmpty)

          // Renaming params to add to clojure
          val nextVar =
            if ((env ++ newParams).isEmpty) 0
            else (env ++ newParams).map(_.root.literal.toString match {
              case a: String if a.startsWith(replacemnetVarPrefix) => a.drop(replacemnetVarPrefix.length).toInt
              case _ => -1
            }).max + 1

          val toAddParamsDefinitions: List[(Term, Term)] = t.subtrees(1).leaves.zip(Stream.from(nextVar)).map(i =>
            (new Identifier("?" + i._1.root.literal.toString, i._1.root.kind, i._1.root.ns),
              new Identifier(replacemnetVarPrefix + i._2, i._1.root.kind, i._1.root.ns))
          ).map(i => (TREE(i._1), TREE(i._2))).filter(env contains _._1).toList

          val toAddParamsUses: List[(Term, Term)] = toAddParamsDefinitions.map(i =>
            (new Identifier(i._1.root.literal.toString.drop(1), i._1.root.kind, i._1.root.ns),
              new Identifier(i._2.root.literal.toString.drop(1), i._2.root.kind, i._2.root.ns))
          ).map(i => (TREE(i._1), TREE(i._2)))

          // Updating AST to include clojure
          val paramsTree = {
            if (toAddParamsDefinitions.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, toAddParamsDefinitions.map(_._2) ++ t.subtrees(0).subtrees)
              case i: Identifier => TREE(Language.tupleId, toAddParamsDefinitions.map(_._2) :+ t.subtrees(0))
            }
          }

          val updatedLambda = TREE(
            Language.lambdaId,
            List(paramsTree, applyClojure(newParams ++ env, t.subtrees(1).replaceDescendants(toAddParamsUses)))
          )

          if (toAddParamsDefinitions.isEmpty) updatedLambda
          else TREE(Language.applyId, updatedLambda +: toAddParamsUses.map(i => i._1))
        case i: Identifier => TREE(i, t.subtrees map (s => applyClojure(env, s)))
      }
    }

    applyClojure(Seq.empty, t)
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