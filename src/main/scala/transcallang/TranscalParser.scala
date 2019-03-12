package transcallang

import com.typesafe.scalalogging.LazyLogging
import syntax.AstSugar._
import syntax.{Identifier, Tree}
import Language._
import synthesis.Programs
import transcallang.Tokens.{GE, GT, LE, LT, SETDISJOINT, SETIN, SETNOTIN, WorkflowToken, _}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}


class TranscalParser extends Parsers with LazyLogging with Parser[Term] with TermInfixer {
  override type Elem = WorkflowToken

  class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
    override def first: WorkflowToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)
  }

  override def log[T](p: => Parser[T])(name: String): Parser[T] = p

  def apply(programText: String): Term = {
    // Clean comments and new lines inside parenthesis
    // TODO: replace comments with whitespace for receiving errorl location correctly
    def cleanLineComments(text: String): String = "(.*?)(//.+)?".r.replaceAllIn(text, m => m.group(1))

    def cleanMultilineComments(text: String): String = "/\\*(.|\n)*?\\*/".r.replaceAllIn(text, "")

    val text = cleanMultilineComments(programText).split("\n").map(cleanLineComments).mkString("\n")
    val tokens = Lexer.apply(text)
    if (tokens.isLeft) throw new RuntimeException(s"LEXER ERRoR: ${tokens.left.get}")
    val reader = new WorkflowTokenReader(tokens.right.get)
    program(reader) match {
      case Success(matched, rt) => matched
      case Failure(msg, rt) =>
        throw new RuntimeException(s"FAILURE: $msg in ${rt}")
      case Error(msg, rt) =>
        throw new RuntimeException(s"ERROR: $msg in ${rt}")
    }
  }

  private def TREE(x: Identifier, subtrees: List[Tree[Identifier]] = List.empty): Term = T(x, subtrees)

  // Example of defining a parser for a word
  // def word: Parser[String]    = """[a-z]+""".r ^^ { _.toString }

  // A little explanation for future students of Shachar
  // This system uses combinators to create the EBNF.
  // ~ means one after another.
  // | is options
  // ^^ we finished our regex/parser now what we do with it
  // You can google the rest.

  // Bracet shortcuts:
  private val RBO = ROUNDBRACETOPEN()
  private val RBC = ROUNDBRACETCLOSE()
  private val SBC = SQUAREBRACETCLOSE()
  private val SBO = SQUAREBRACETOPEN()
  private val CBC = CURLYBRACETCLOSE()
  private val CBO = CURLYBRACETOPEN()

  private def identifierLiteral: Parser[Term] = accept("identifier", {
    case IDENTIFIER(name) if Language.identifierRegex.unapplySeq(name).isDefined => TREE(I(name))
    case HOLE() => TREE(Language.holeId)
  })

  private def typeLiteral: Parser[Term] = accept("type", {
    case IDENTIFIER(name) if Language.typeRegex.unapplySeq(name).isDefined => TREE(I(name))
    case HOLE() => TREE(Language.holeId)
  })

  private def number: Parser[Term] = accept("number", { case NUMBER(x) => TREE(I(x)) })

  private def literal: Parser[Term] = accept("string literal", { case LITERAL(name) => TREE(Language.stringLiteralId, List(TREE(I(name)))) })


  def polymorphicTypes: Parser[Term] = (typeLiteral ~ log((LT() ~> types <~ GT()).?)("polymorphic type")) ^^ {
    case x ~ None => x
    case x ~ Some(polymorphic) => TREE(Language.innerTypeId, List(x, polymorphic))
  }

  def types: Parser[Term] = (polymorphicTypes ~ log((MAPTYPE() ~> types).?)("getting map type def")) ^^ {
    case x ~ None => x
    case x ~ Some(recursive) => TREE(Language.mapTypeId, List(x, recursive))
  }

  def identifier: Parser[Term] = (identifierLiteral ~ log((COLON() ~> types).?)("type def")) ^^ { i =>
    i match {
      case (x: Term) ~ None => x
      case (x: Term) ~ Some(z) => TREE(Language.typeBuilderId, List(x, z))
    }
  }

  def consts: Parser[Term] = (NIL() | TRUE() | FALSE()) ^^ {
    case NIL() => TREE(Language.nilId)
    case TRUE() => TREE(Language.trueId)
    case FALSE() => TREE(Language.falseId)
  }

  def tuple: Parser[Term] = ((RBO ~> COMMA() <~ RBC) |
    (RBO ~> (expression <~ COMMA()).+ ~ expression.? <~ RBC)) ^^ { x =>
    val subtrees = x match {
      case (others: scala.List[Term]) ~ (one: Option[Term]) =>
        logger.trace(s"found tuple - $others, $one")
        one.map(others :+ _) getOrElse others
      case COMMA() => List.empty
    }
    TREE(Language.tupleId, subtrees)
  }

  def exprValuesAndParens: Parser[Term] = (tuple | (RBO ~> expression <~ RBC) | (CBO ~ expression ~ CBC) | number | consts | identifier) ^^ {
    case m: Term => m
    case CBO ~ t ~ CBC => TREE(Language.setId, List(t.asInstanceOf[Term]))
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

  def exprNot: Parser[Term] = (rep(NOT()) ~ exprApply) ^^ { x =>
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

  def guarded: Parser[Term] = ((RBO ~> guarded <~ RBC) | ((exprInfixOperator <~ log(GUARDED())("guarded")) ~ expression)) ^^ (x => {
    logger.trace(s"found guarded - $x")
    x match {
      case (x1: Term) ~ (x2: Term) => TREE(Language.guardedId, List(x1, x2))
      case x: Term => x
    }
  })

  def splitted: Parser[List[Term]] = ((RBO ~> splitted <~ RBC)| (guarded ~ (BACKSLASH() ~> guarded).+)) ^^ {
    case x: List[Term] => x
    case x: (Term ~ List[Term]) => x._1 +: x._2
  }

  def expression: Parser[Term] = (((exprInfixOperator <~ log(MATCH())("match")) ~! splitted) | exprInfixOperator) ^^ {
    case (matched: Term) ~ (terms: List[Term]) =>
      logger.trace(s"found match expression $matched ~ $terms")
      replaceUnicode(TREE(Language.matchId, List(matched) ++ terms))
    case x: Term =>
      logger.trace(s"found expression $x")
      replaceUnicode(x)
  }

  def annotation: Parser[Term] = accept("annotation", { case ANNOTATION(name) => TREE(I(name)) })

  def statementCommand: Parser[Term] = (expression ~ log(RIGHTARROW())("tactic statement") ~! expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement expr - $x")
    x match {
      case left ~ dir ~ right ~ anno =>
        val definitionTerm = new Tree(Language.tacticId, List(left, right))
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statementDefinition: Parser[Term] = ((expression <~ TRUECONDBUILDER()).? ~ expression ~ log(LET() | DIRECTEDLET())("let statement") ~! expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement let - $x")
    x match {
      case op ~ left ~ defdir ~ right ~ anno =>
        // This means lhs is a function
        val (fixedLeft, fixedRight) = right.root match {
          case Language.lambdaId =>
            val rightParams = if (right.subtrees(0).root == Language.tupleId) right.subtrees(0).subtrees else List(right.subtrees(0))
            val newRight = right.subtrees(1)
            val newLeft = TREE(left.root, left.subtrees ++ rightParams)
            (newLeft, newRight)
          case _ => (left, right)
        }
        val conditionedLeft = op.map(o => TREE(trueCondBuilderId, List(o, fixedLeft))).getOrElse(fixedLeft)
        val dir = if (defdir == LET()) Language.letId else Language.directedLetId
        val definitionTerm = TREE(dir, List(conditionedLeft, fixedRight))
        anno.map(a => new Tree(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statement: Parser[Term] = (statementDefinition | statementCommand) ^^ { t =>
    logger.debug(s"statement - ${Programs.termToString(t)} $t")

    def applyClojure(env: Seq[Identifier], t: Term): Term = {
      val replacemnetVarPrefix = "?autovar"
      def getReplacmentParams(newAutovars: List[Identifier]): (List[(Identifier, Identifier)], List[(Identifier, Identifier)]) = {
        val nextVar =
          if ((env ++ newAutovars).isEmpty) 0
          else (env ++ newAutovars).map(_.literal.toString match {
            case a: String if a.startsWith(replacemnetVarPrefix) => a.drop(replacemnetVarPrefix.length).toInt
            case _ => -1
          }).max + 1

        val toAddParamsDefinitions: List[(Identifier, Identifier)] = t.subtrees.tail.flatMap(_.terminals).zip(Stream.from(nextVar)).map(i =>
          (new Identifier("?" + i._1.literal.toString, i._1.kind, i._1.ns),
            new Identifier(replacemnetVarPrefix + i._2, i._1.kind, i._1.ns))
        ).filter(env contains _._1)

        val toAddParamsUses: List[(Identifier, Identifier)] = toAddParamsDefinitions.map(i =>
          (new Identifier(i._1.literal.toString.drop(1), i._1.kind, i._1.ns),
            new Identifier(i._2.literal.toString.drop(1), i._2.kind, i._2.ns)))

        (toAddParamsDefinitions, toAddParamsUses)
      }

      def replaceIdentifiers(term: Term, paramsUses: Map[Identifier, Identifier]): Term = {
        if (paramsUses.contains(term.root))
          if (term.isLeaf) TREE(paramsUses(term.root))
          else TREE(Language.applyId, TREE(paramsUses(term.root)) +: term.subtrees.map(replaceIdentifiers(_, paramsUses)))
        else TREE(term.root, term.subtrees.map(replaceIdentifiers(_, paramsUses)))
      }

      t.root match {
        case Language.letId | Language.directedLetId =>
          val newParams = t.subtrees(0).terminals.filter(_.literal.toString.startsWith("?"))
          assert(env.intersect(newParams).isEmpty)
          TREE(t.root, List(t.subtrees(0), applyClojure(newParams ++ env, t.subtrees(1))))
        case Language.matchId =>
          // Renaming params to add to clojure
          val newAutovars = t.subtrees.tail.flatMap(_.subtrees.head.terminals).filter(_.literal.toString.startsWith("?"))

          val (toAddParamsDefinitions, toAddParamsUses) = getReplacmentParams(newAutovars)

          // Updating AST to include clojure
          val matchCallParams = {
            if (toAddParamsDefinitions.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, toAddParamsUses.map(i => TREE(i._1)) ++ t.subtrees(0).subtrees)
              case i: Identifier => TREE(Language.tupleId, toAddParamsUses.map(i => TREE(i._1)) :+ t.subtrees(0))
            }
          }

          val newGuarded = t.subtrees.tail.map(st => {
            // root should always be guarded
            val newMatchedTree = st.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, toAddParamsDefinitions.map(i => TREE(i._2)) ++ st.subtrees(0).subtrees)
              case i: Identifier =>
                if (toAddParamsDefinitions.nonEmpty) TREE(Language.tupleId, toAddParamsDefinitions.map(i => TREE(i._2)) :+ st.subtrees(0))
                else st.subtrees(0)
            }

            val newParams = st.subtrees(0).terminals.filter(_.literal.toString.startsWith("?"))
            assert(env.intersect(newParams).isEmpty)

            val useMap = toAddParamsUses.toMap
            TREE(st.root, List(newMatchedTree, applyClojure(newParams ++ toAddParamsDefinitions.map(_._2) ++ env, replaceIdentifiers(st.subtrees(1), useMap))))
          })

          TREE(
            Language.matchId,
            matchCallParams +: newGuarded
          )
        case Language.lambdaId =>
          val newParams = t.subtrees(0).terminals.filter(_.literal.toString.startsWith("?")).toList
          assert(env.intersect(newParams).isEmpty)

          val (toAddParamsDefinitions, toAddParamsUses) = getReplacmentParams(newParams)

          // Updating AST to include clojure
          val paramsTree = {
            if (toAddParamsDefinitions.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              case Language.tupleId => TREE(Language.tupleId, toAddParamsDefinitions.map(i => TREE(i._2)) ++ t.subtrees(0).subtrees)
              case i: Identifier => TREE(Language.tupleId, toAddParamsDefinitions.map(i => TREE(i._2)) :+ t.subtrees(0))
            }
          }

          val useMap = toAddParamsUses.toMap
          val updatedLambda = TREE(
            Language.lambdaId,
            List(paramsTree, applyClojure(newParams ++ toAddParamsDefinitions.map(_._2) ++ env, replaceIdentifiers(t.subtrees(1), useMap)))
          )

          if (toAddParamsDefinitions.isEmpty) updatedLambda
          else TREE(Language.applyId, updatedLambda +: toAddParamsUses.map(i => TREE(i._1)))
        case i: Identifier => TREE(i, t.subtrees map (s => applyClojure(env, s)))
      }
    }

    applyClojure(Seq.empty, t)
  }

  def commands: Parser[Term] = (RIGHTARROW() | LEFTARROW() | SBO ~ SBC | SQUARE()) ^^ {
    x =>
      logger.debug(s"command - $x")
      new Tree(Language.commandId, List(TREE(new Identifier(x match {
        case RIGHTARROW() => "<-"
        case LEFTARROW() => "->"
        case SBO ~ SBC | SQUARE() => "[]"
      }))))
  }

  def program: Parser[Term] = phrase(SEMICOLON().* ~> (statement | commands) ~ rep(SEMICOLON().+ ~> (statement | commands).?)) ^^ {
    case sc ~ scCommaList => scCommaList.filter(_.nonEmpty).map(_.get).foldLeft(sc)((t1, t2) => new Tree(semicolonId, List(t1, t2)))
  }

  /** The known left operators at the moment */
  override def lefters: Map[Int, Set[WorkflowToken]] = Map(
    (Infixer.MIDDLE - 1, Set(PLUS(), PLUSPLUS(), MINUS(), UNION())),
    (Infixer.MIDDLE, Set(SNOC())),
    (Infixer.MIDDLE + 1, Set(EQUALS(), NOTEQUALS(), SETIN(), SETNOTIN(), SETDISJOINT(), LT(), LE(), GE(), GT())),
    (Infixer.MIDDLE + 2, Set(AND())),
    (Infixer.MIDDLE + 3, Set(OR())),
    //    (Infixer.MIDDLE + 4, builtinIFFOps.toSet),
    (Infixer.MIDDLE + 5, Set(ANDCONDBUILDER(), LAMBDA()))
  )

  /** The known right operators at the moment */
  override def righters: Map[Int, Set[WorkflowToken]] = Map(
    (Infixer.MIDDLE, Set(DOUBLECOLON()))
  )

  /** A way to rebuild the the class */
  override def build(lefters: Map[Int, Set[WorkflowToken]], righters: Map[Int, Set[WorkflowToken]]): TermInfixer =
    throw new NotImplementedError()
}