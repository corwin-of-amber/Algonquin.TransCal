package transcallang

import com.typesafe.scalalogging.LazyLogging
import synthesis.Programs
import transcallang.Language._
import transcallang.Tokens.{GE, GT, LE, LT, SETDISJOINT, SETIN, SETNOTIN, WorkflowToken, _}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}


class TranscalParser extends Parsers with LazyLogging with Parser[AnnotatedTree] with TermInfixer {
  override type Elem = WorkflowToken

  class WorkflowTokenReader(tokens: Seq[WorkflowToken]) extends Reader[WorkflowToken] {
    override def first: WorkflowToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[WorkflowToken] = new WorkflowTokenReader(tokens.tail)

    override def toString: String = s"WorkflowTokenReader(pos=${pos.column}, tokens=${tokens.mkString(", ")})"
  }

  override def log[T](p: => Parser[T])(name: String): Parser[T] = p

  def apply(programText: String): AnnotatedTree = {
    // Clean comments and new lines inside parenthesis
    // TODO: replace comments with whitespace for receiving errorl location correctly
    def cleanLineComments(text: String): String = "(.*?)(//.+)?".r.replaceAllIn(text, m => m.group(1))

    def cleanMultilineComments(text: String): String = "/\\*(.|\n)*?\\*/".r.replaceAllIn(text, "")

    val text = cleanMultilineComments(programText).split("\n").map(cleanLineComments).mkString("\n")
    val tokens = Lexer.apply(text)
    if (tokens.isLeft) throw new RuntimeException(s"LEXER ERROR: ${tokens.left.get}")
    val reader = new WorkflowTokenReader(tokens.right.get)
    program(reader) match {
      case Success(matched, _) => matched
      case Failure(msg, rt) =>
        throw new RuntimeException(s"FAILURE: $msg in $rt")
      case Error(msg, rt) =>
        throw new RuntimeException(s"ERROR: $msg in $rt")
    }
  }

  def parseExpression(programText: String): AnnotatedTree = {
    def cleanLineComments(text: String): String = "(.*?)(//.+)?".r.replaceAllIn(text, m => m.group(1))
    val text = cleanLineComments(programText)
    val tokens = Lexer.apply(text)
    val reader = new WorkflowTokenReader(tokens.right.get)
    expression(reader).get
  }

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

  private def identifierLiteral: Parser[AnnotatedTree] = accept("identifier", {
    case IDENTIFIER(name) if Language.identifierRegex.unapplySeq(name).isDefined => AnnotatedTree.identifierOnly(Identifier(name))
    case HOLE() => AnnotatedTree.identifierOnly(Language.holeId)
  })

  private def number: Parser[AnnotatedTree] = accept("number", { case NUMBER(x) => AnnotatedTree.identifierOnly(Identifier(x.toString, annotation = Some(AnnotatedTree.identifierOnly(Identifier("int"))))) })

  def types: Parser[AnnotatedTree] = (exprValuesAndParens ~ log((MAPTYPE() ~> exprValuesAndParens).*)("getting map type def")) ^^ {
    case x ~ Nil => x
    case x ~ list => AnnotatedTree.withoutAnnotations(Language.mapTypeId, x +: list)
  }

  def identifier: Parser[AnnotatedTree] = (identifierLiteral ~ log((COLON() ~> types).?)("type def")) ^^ {
    case (x: AnnotatedTree) ~ None => x
    case (x: AnnotatedTree) ~ Some(z) => x.copy(root = x.root.copy(annotation = Some(z)))
  }

  def consts: Parser[AnnotatedTree] = (NIL() | TRUE() | FALSE()) ^^ {
    case NIL() => AnnotatedTree.identifierOnly(Language.nilId)
    case TRUE() => AnnotatedTree.identifierOnly(Language.trueId)
    case FALSE() => AnnotatedTree.identifierOnly(Language.falseId)
  }

  def tuple: Parser[AnnotatedTree] = ((RBO ~> COMMA() <~ RBC) |
    (RBO ~> (expression <~ COMMA()).+ ~ expression.? <~ RBC)) ^^ { x =>
    val subtrees = x match {
      case (others: scala.List[AnnotatedTree]) ~ (one: Option[AnnotatedTree]) =>
        logger.trace(s"found tuple - $others, $one")
        one.map(others :+ _) getOrElse others
      case COMMA() => Seq.empty
    }
    AnnotatedTree.withoutAnnotations(Language.tupleId, subtrees)
  }

  def exprValuesAndParens: Parser[AnnotatedTree] = (tuple | (RBO ~> expression <~ RBC) | (CBO ~ expression ~ CBC) | number | consts | identifier) ^^ {
    case m: AnnotatedTree => m
    case CBO ~ t ~ CBC => AnnotatedTree.withoutAnnotations(Language.setId, Seq(t.asInstanceOf[AnnotatedTree]))
  }

  def exprApply: Parser[AnnotatedTree] = rep1(exprValuesAndParens) ^^ { applied =>
    if (applied.tail.isEmpty) applied.head
    else {
      logger.trace(s"apply - $applied")
      applied.tail match {
        case t: Seq[AnnotatedTree] if t.head.root == Language.tupleId && t.size == 1 => applied.head.copy(subtrees=applied.head.subtrees ++ applied.last.subtrees)
        case _ => applied.head.copy(subtrees=applied.head.subtrees ++ applied.drop(1))
      }
    }
  }

  def exprNot: Parser[AnnotatedTree] = (rep(NOT()) ~ exprApply) ^^ { x =>
    if (x._1.nonEmpty) logger.trace(s"not - $x")
    x match {
      case applied ~ exp => applied.foldLeft(exp)((t, _) => AnnotatedTree.withoutAnnotations(Language.negId, List(t)))
    }
  }

  def exprInfixOperator: Parser[AnnotatedTree] = operatorsParser(exprNot)

  private val normalToUnicode: Map[String, String] = Map("\\/" -> "∨", "/\\" -> "∧", "!=" -> "≠", "||" -> "‖", "<=" -> "≤", ">=" -> "≥", "=>" -> "⇒")

  private def translateUnicode(t: String): String = normalToUnicode.getOrElse(t, t)

  private def replaceUnicode(t: AnnotatedTree): AnnotatedTree = t match {
    case t: AnnotatedTree if t.root == Language.stringLiteralId => t
    case t: AnnotatedTree => t.copy(root=t.root.copy(translateUnicode(t.root.literal)), subtrees=t.subtrees.map(replaceUnicode))
  }

  def guarded: Parser[AnnotatedTree] = ((RBO ~> guarded <~ RBC) | ((exprInfixOperator <~ log(GUARDED())("guarded")) ~ expression)) ^^ (x => {
    logger.trace(s"found guarded - $x")
    x match {
      case (x1: AnnotatedTree) ~ (x2: AnnotatedTree) => AnnotatedTree.withoutAnnotations(Language.guardedId, List(x1, x2))
      case x: AnnotatedTree => x
    }
  })

  def splitted: Parser[List[AnnotatedTree]] = ((RBO ~> splitted <~ RBC)| (guarded ~ (BACKSLASH() ~> guarded).+)) ^^ {
    case x: List[AnnotatedTree] => x
    case x: (AnnotatedTree ~ List[AnnotatedTree]) => x._1 +: x._2
  }

  def expression: Parser[AnnotatedTree] = (((exprInfixOperator <~ log(MATCH())("match")) ~! splitted) | exprInfixOperator) ^^ {
    case (matched: AnnotatedTree) ~ (terms: List[AnnotatedTree]) =>
      logger.trace(s"found match expression $matched ~ $terms")
      replaceUnicode(AnnotatedTree.withoutAnnotations(Language.matchId, List(matched) ++ terms))
    case x: AnnotatedTree =>
      logger.trace(s"found expression $x")
      replaceUnicode(x)
  }

  // TODO: we should insert annotation into tree node.
  def annotation: Parser[AnnotatedTree] = accept("annotation", { case ANNOTATION(name) => AnnotatedTree.identifierOnly(Identifier(name)) })

  // TODO: we should insert annotation into tree node.
  def statementCommand: Parser[AnnotatedTree] = (expression ~ log(RIGHTARROW())("tactic statement") ~! expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement expr - $x")
    x match {
      case left ~ _ ~ right ~ anno =>
        val definitionTerm = AnnotatedTree.withoutAnnotations(Language.tacticId, List(left, right))
        anno.map(a => AnnotatedTree.withoutAnnotations(Language.annotationId, List(definitionTerm, a))) getOrElse definitionTerm
    }
  }

  def statementDefinition: Parser[AnnotatedTree] = ((expression <~ TRUECONDBUILDER()).? ~ expression ~ log(LET() | DIRECTEDLET() | LIMITEDLET() | LIMITEDDIRECTEDLET())("let statement") ~! expression ~ annotation.?) ^^ { x =>
    logger.trace(s"statement let - $x")
    x match {
      case op ~ left ~ defdir ~ right ~ anno =>
        // This means lhs is a function
        val (fixedLeft, fixedRight) = right.root match {
          case Language.lambdaId =>
            // Doing some lambda param fixing
            val rightParams = if (right.subtrees(0).root == Language.tupleId) right.subtrees(0).subtrees else List(right.subtrees(0))
            val newRight = right.subtrees(1)
            val newLeft = AnnotatedTree(left.root, left.subtrees ++ rightParams, Seq.empty)
            (newLeft, newRight)
          case _ => (left, right)
        }
        val conditionedLeft = op.map(o => AnnotatedTree(trueCondBuilderId, List(o, fixedLeft), Seq.empty)).getOrElse(fixedLeft)
        val dir = defdir.toIdentifier
        // TODO: we should insert annotation into tree node.
        val definitionTerm = AnnotatedTree(dir, List(conditionedLeft, fixedRight), Seq.empty)
        anno.map(a => AnnotatedTree(Language.annotationId, List(definitionTerm, a), Seq.empty)) getOrElse definitionTerm
    }
  }

  def statement: Parser[AnnotatedTree] = (statementDefinition | statementCommand) ^^ { t =>
    logger.debug(s"statement - ${Programs.termToString(t)} $t")

    def applyClojure(env: Seq[Identifier], t: AnnotatedTree): AnnotatedTree = {
      val replacemnetVarPrefix = "?autovar"

      def getReplacmentParams(newAutovars: Seq[Identifier]): (Seq[(Identifier, Identifier)], Seq[(Identifier, Identifier)]) = {
        val nextVar = (env ++ newAutovars)
          .map(_.literal)
          .filter(_.startsWith(replacemnetVarPrefix)).map(_.drop(replacemnetVarPrefix.length).toInt)
          .sorted.lastOption.getOrElse(-1) + 1

        val toAddParamsDefinitions: Seq[(Identifier, Identifier)] = t.subtrees.tail.flatMap(_.terminals).zip(Stream.from(nextVar)).map(i =>
          (i._1.copy(literal="?" + i._1.literal),
            i._1.copy(literal=replacemnetVarPrefix + i._2))
        ).filter(env contains _._1)

        val toAddParamsUses: Seq[(Identifier, Identifier)] = toAddParamsDefinitions.map(i =>
          (i._1.copy(i._1.literal.drop(1)),
            i._2.copy(i._2.literal.drop(1))))

        (toAddParamsDefinitions, toAddParamsUses)
      }

      def replaceIdentifiers(term: AnnotatedTree, paramsUses: Map[Identifier, Identifier]): AnnotatedTree = {
        if (paramsUses.contains(term.root))
          if (term.isLeaf) term.map(i => paramsUses.getOrElse(i, i))
          // TODO: Why are we inserting apply?
          else AnnotatedTree(Language.applyId, AnnotatedTree.identifierOnly(paramsUses(term.root)) +: term.subtrees.map(replaceIdentifiers(_, paramsUses)), Seq.empty)
        else term.copy(subtrees=term.subtrees.map(replaceIdentifiers(_, paramsUses)))
      }

      t.root match {
        case Language.letId | Language.directedLetId =>
          val newParams = t.subtrees(0).terminals.filter(_.literal.toString.startsWith("?"))
          assert(env.intersect(newParams).isEmpty)
          t.copy(subtrees=List(t.subtrees(0), applyClojure(newParams ++ env, t.subtrees(1))))
        case Language.matchId =>
          // Renaming params to add to clojure
          val newAutovars = t.subtrees.tail.flatMap(_.subtrees.head.terminals).filter(_.literal.toString.startsWith("?"))

          val (toAddParamsDefinitions, toAddParamsUses) = getReplacmentParams(newAutovars)

          // Updating AST to include clojure
          val matchCallParams = {
            if (toAddParamsDefinitions.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              // TODO: if we have annotation changing values might change annotation?
              case Language.tupleId => t.copy(subtrees=toAddParamsUses.map(i => AnnotatedTree.identifierOnly(i._1)) ++ t.subtrees(0).subtrees)
              case _: Identifier => AnnotatedTree(Language.tupleId, toAddParamsUses.map(i => AnnotatedTree.identifierOnly(i._1)) :+ t.subtrees(0), Seq.empty)
            }
          }

          val newGuarded = t.subtrees.tail.map(st => {
            // root should always be guarded
            val newMatchedTree = st.subtrees(0).root match {
              // TODO: if we have annotation changing values might change annotation?
              case Language.tupleId => st.subtrees(0).copy(subtrees=toAddParamsDefinitions.map(i => AnnotatedTree.identifierOnly(i._2)) ++ st.subtrees(0).subtrees)
              case _: Identifier =>
                if (toAddParamsDefinitions.nonEmpty) AnnotatedTree(Language.tupleId, toAddParamsDefinitions.map(i => AnnotatedTree.identifierOnly(i._2)) :+ st.subtrees(0), Seq.empty)
                else st.subtrees(0)
            }

            val newParams = st.subtrees(0).terminals.filter(_.literal.toString.startsWith("?"))
            assert(env.intersect(newParams).isEmpty)

            val useMap = toAddParamsUses.toMap
            st.copy(subtrees=List(newMatchedTree, applyClojure(newParams ++ toAddParamsDefinitions.map(_._2) ++ env, replaceIdentifiers(st.subtrees(1), useMap))))
          })

          AnnotatedTree(
            Language.matchId,
            matchCallParams +: newGuarded,
            t.annotations
          )
        case Language.lambdaId =>
          val newParams = t.subtrees(0).terminals.filter(_.literal.toString.startsWith("?")).toList
          assert(env.intersect(newParams).isEmpty)

          val (toAddParamsDefinitions, toAddParamsUses) = getReplacmentParams(newParams)

          // Updating AST to include clojure
          val paramsTree = {
            if (toAddParamsDefinitions.isEmpty) t.subtrees(0)
            else t.subtrees(0).root match {
              case Language.tupleId => t.subtrees(0).copy(subtrees=toAddParamsDefinitions.map(i => AnnotatedTree.identifierOnly(i._2)) ++ t.subtrees(0).subtrees)
              case _: Identifier => AnnotatedTree(Language.tupleId, toAddParamsDefinitions.map(i => AnnotatedTree.identifierOnly(i._2)) :+ t.subtrees(0), Seq.empty)
            }
          }

          val useMap = toAddParamsUses.toMap
          val updatedLambda = AnnotatedTree(
            Language.lambdaId,
            List(paramsTree, applyClojure(newParams ++ toAddParamsDefinitions.map(_._2) ++ env, replaceIdentifiers(t.subtrees(1), useMap))),
            Seq.empty
          )

          if (toAddParamsDefinitions.isEmpty) updatedLambda
          else AnnotatedTree(Language.applyId, updatedLambda +: toAddParamsUses.map(i => AnnotatedTree.identifierOnly(i._1)), Seq.empty)
        case i: Identifier => AnnotatedTree(i, t.subtrees map (s => applyClojure(env, s)), Seq.empty)
      }
    }

    applyClojure(Seq.empty, t)
  }

  def commands: Parser[AnnotatedTree] = (RIGHTARROW() | LEFTARROW() | SBO ~ SBC | SQUARE()) ^^ {
    x =>
      logger.debug(s"command - $x")
      AnnotatedTree(Language.commandId, List(AnnotatedTree.identifierOnly(Identifier(x match {
        case RIGHTARROW() => "<-"
        case LEFTARROW() => "->"
        case SBO ~ SBC | SQUARE() => "[]"
      }))), Seq.empty)
  }

  def program: Parser[AnnotatedTree] = phrase(SEMICOLON().* ~> (statement | commands) ~ rep(SEMICOLON().+ ~> (statement | commands).?)) ^^ {
    case sc ~ scCommaList => scCommaList.filter(_.nonEmpty).map(_.get).foldLeft(sc)((t1, t2) => AnnotatedTree(semicolonId, List(t1, t2), Seq.empty))
  }

  /** The known left operators at the moment */
  override def lefters: Map[Int, Set[WorkflowToken]] = Map(
    (Infixer.MIDDLE - 1, Set(PLUS(), PLUSPLUS(), MINUS(), UNION())),
    (Infixer.MIDDLE, Set(SNOC())),
    (Infixer.MIDDLE + 1, Set(EQUALS(), NOTEQUALS(), SETIN(), SETNOTIN(), SETDISJOINT(), LT(), LE(), GE(), GT())),
    (Infixer.MIDDLE + 2, Set(AND())),
    (Infixer.MIDDLE + 3, Set(OR())),
    //    (Infixer.MIDDLE + 4, builtinIFFOps.toSet),
    (Infixer.MIDDLE + 5, Set(LIMITEDANDCONDBUILDER(), ANDCONDBUILDER(), LAMBDA()))
  )

  /** The known right operators at the moment */
  override def righters: Map[Int, Set[WorkflowToken]] = Map(
    (Infixer.MIDDLE, Set(DOUBLECOLON()))
  )

  /** A way to rebuild the the class */
  override def build(lefters: Map[Int, Set[WorkflowToken]], righters: Map[Int, Set[WorkflowToken]]): TermInfixer =
    throw new NotImplementedError()
}