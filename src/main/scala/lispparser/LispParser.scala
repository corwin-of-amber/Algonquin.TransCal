package lispparser

import com.fasterxml.jackson.databind.introspect.AnnotatedMethodMap
import com.typesafe.scalalogging.LazyLogging
import lispparser.Lexer.{IDENTIFIER, LITERAL, ROUNDBRACETCLOSE, ROUNDBRACETOPEN, Token}
import transcallang.{AnnotatedTree, Identifier, Language}

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.{NoPosition, Position, Reader}

class LispParser extends RegexParsers with LazyLogging with transcallang.Parser[List[AnnotatedTree]] {
  override type Elem = Token

  class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[Token] = new TokenReader(tokens.tail)

    override def toString: String = s"WorkflowTokenReader(pos=${pos.column}, tokens=${tokens.mkString(", ")})"
  }

  override def log[T](p: => Parser[T])(name: String): Parser[T] = p

  def apply(programText: String): List[AnnotatedTree] = {
    val tokens = Lexer.apply(programText)
    if (tokens.isLeft) throw new RuntimeException(s"LEXER ERROR: ${tokens.left.get}")
    val reader = new TokenReader(tokens.right.get)
    program(reader) match {
      case Success(matched, _) =>
        assert(!matched.forall(_.nodes.map(_.root.literal).contains("ite")))
        matched
      case Failure(msg, rt) =>
        throw new RuntimeException(s"FAILURE: $msg in $rt")
      case Error(msg, rt) =>
        throw new RuntimeException(s"ERROR: $msg in $rt")
    }
  }

  val identifierGenerator: Iterator[Identifier] = Stream.from(0).map(i => Identifier(s"autovar$i")).toIterator

  private val RBO = ROUNDBRACETOPEN()
  private val RBC = ROUNDBRACETCLOSE()

  def program: Parser[List[AnnotatedTree]] = (((assertTerm | functionDecl) ^^ { a => List(a) } | functionDef | datatypes).* ~ assertTerm <~ RBO <~ IDENTIFIER("check-sat") <~ RBC) ^^ parseProgram

  def numeral: Parser[AnnotatedTree] = ("0" | "[1-9][0-9]*".r) ^^ parseNumeral

  def decimal: Parser[AnnotatedTree] = numeral ~ "\\.0+".r ~ numeral ^^ parseDecimal

  def hexdecimal: Parser[AnnotatedTree] = "#x" ~> "[a-fA-F1-9][a-fA-F0-9]*".r ^^ {
    throw new RuntimeException("hex not supported yet")
  }

  def binary: Parser[AnnotatedTree] = "#b" ~> "[01]+".r ^^ {
    throw new RuntimeException("binary not supported yet")
  }

  def string: Parser[AnnotatedTree] = accept("literal", {
    case LITERAL(name) => AnnotatedTree.withoutAnnotations(Language.stringLiteralId, List(AnnotatedTree.identifierOnly(Identifier(name))))
  })

  def symbol: Parser[AnnotatedTree] = simple_symbol | ("\\|[^|\\\\]*|".r ^^ { a => AnnotatedTree.identifierOnly(Identifier(a)) })

  def keyword: Parser[AnnotatedTree] = ":" ~> simple_symbol

  def datatype: Parser[AnnotatedTree] = (RBO ~> IDENTIFIER("declare-datatype") ~> symbol ~ datatypeDecl <~ RBC) ^^ parseDatatype

  def datatypes: Parser[List[AnnotatedTree]] = (RBO ~> IDENTIFIER("declare-datatypes") ~> RBO ~> RBC ~> RBO ~> datatype.* <~ RBC <~ RBC)

  def functionDecl: Parser[AnnotatedTree] = (RBO ~> IDENTIFIER("declare-fun") ~> symbol ~ RBO ~ sort.* ~ RBC ~ sort <~ RBC) ^^ parseFunctionDecl

  def functionDef: Parser[List[AnnotatedTree]] = (RBO ~> IDENTIFIER("define-fun") ~> symbol ~ RBO ~ sortedVar.* ~ RBC ~ sort ~ term <~ RBC) ^^ parseFunctionDef

  def specConstant: Parser[AnnotatedTree] = numeral | decimal | hexdecimal | binary | string

  def sexp: Parser[AnnotatedTree] = specConstant | symbol | keyword | (RBO ~> sexp.* <~ RBC) ^^ parseSExp

  def index: Parser[AnnotatedTree] = numeral | symbol

  def identifier: Parser[AnnotatedTree] = symbol | (RBO ~> "_" ~> symbol ~ index.+ <~ RBC) ^^ parseIndexedIdentifier

  def sort: Parser[AnnotatedTree] = identifier | (RBO ~> identifier ~ sort.+ <~ RBC) ^^ parseSort

  def simple_symbol: Parser[AnnotatedTree] = "[a-zA-Z+\\-/*=%?!.$_̃ &ˆ<>@][0-9a-zA-Z+\\-/*=%?!.$_̃ &ˆ<>@]+".r ^^ { a => AnnotatedTree.identifierOnly(Identifier(a)) }

  def qualIdentifier: Parser[AnnotatedTree] = identifier | ((RBO ~> IDENTIFIER("as") ~> identifier <~ sort <~ RBC)) //^^ parseQualIdent)

  def sortedVar: Parser[AnnotatedTree] = (RBO ~> symbol ~ sort <~ RBC) ^^ { case symbol ~ sort => symbol.copy(root = symbol.root.copy(annotation = Some(sort))) }

  def varBinding: Parser[AnnotatedTree ~ AnnotatedTree] = (RBO ~> symbol ~ term <~ RBC)

  def pattern: Parser[AnnotatedTree] = symbol | (RBO ~> symbol ~ symbol.+ <~ RBC) ^^ { a => AnnotatedTree.withoutAnnotations(a._1.root, a._2) }

  def matchCase: Parser[AnnotatedTree] = (RBO ~> pattern ~ term <~ RBC) ^^ { a => AnnotatedTree.withoutAnnotations(Language.guardedId, List(a._1, a._2)) }

  def attributeValue: Parser[AnnotatedTree] = specConstant | symbol | (RBO ~> sexp.* <~ RBC) ^^ parseSExp

  def attribute: Parser[AnnotatedTree] = keyword | keyword <~ attributeValue

  def term: Parser[AnnotatedTree] = specConstant | qualIdentifier |
    (RBO ~> qualIdentifier ~ term.+ <~ RBC) ^^ { case i ~ terms => AnnotatedTree.withoutAnnotations(i.root, terms) } |
    (RBO ~> "let" ~ RBO ~> varBinding.+ ~ RBC ~ term <~ RBC) ^^ parseLet |
    (RBO ~> "forall" ~ RBO ~> sortedVar.+ ~ RBC ~ term <~ RBC) ^^ parseForall |
    (RBO ~> "exists" ~ RBO ~> sortedVar.+ ~ RBC ~ term <~ RBC) ^^ parseExists |
    (RBO ~> "match" ~> term ~ RBO ~ matchCase.+ <~ RBC <~ RBC) ^^ parseMatch |
    (RBO ~> "!" ~> term <~ attribute.+ <~ RBC)

  def assertTerm: Parser[AnnotatedTree] = (RBO ~> IDENTIFIER("assert") ~> term <~ RBC) ^^ parseAssert

  def datatypeDecl: Parser[List[AnnotatedTree]] = (RBO ~> constructorDecl.+ <~ RBC)

  def selectorDecl: Parser[AnnotatedTree] = (RBO ~> symbol ~ sort <~ RBC) ^^ parseSelectorDecl

  def constructorDecl: Parser[AnnotatedTree] = (RBO ~> symbol ~ selectorDecl.* <~ RBC) ^^ parseConstructorDecl

  //  def parseNumeral(text: String): AnnotatedTree = AnnotatedTree.withoutAnnotations(Language.numeralId, List(AnnotatedTree.identifierOnly(Identifier(text))))
  def parseNumeral(text: String): AnnotatedTree = AnnotatedTree.identifierOnly(Identifier(text))

  def parseDecimal(matches: this.~[this.~[AnnotatedTree, String], AnnotatedTree]): AnnotatedTree = matches match {
    case n1 ~ zeros ~ n2 => AnnotatedTree.identifierOnly(Identifier(n1.root.literal + zeros + n2.root.literal))
  }

  def parseSExp(obj: Object): AnnotatedTree = obj match {
    case subExps: List[AnnotatedTree] =>
      if (subExps.isEmpty) AnnotatedTree.identifierOnly(Language.tupleId)
      else if (subExps.head.root.literal == "ite")
        AnnotatedTree.withoutAnnotations(Language.matchId, List(
          subExps(1),
          AnnotatedTree.withoutAnnotations(Language.guardedId, List(AnnotatedTree.identifierOnly(Language.trueId), subExps(2))),
          AnnotatedTree.withoutAnnotations(Language.guardedId, List(AnnotatedTree.identifierOnly(Language.falseId), subExps(3)))
        ))
      else if (subExps.head.subtrees.isEmpty) AnnotatedTree.withoutAnnotations(subExps.head.root, subExps.tail)
      else AnnotatedTree.withoutAnnotations(Language.applyId, subExps)
    case identifier: AnnotatedTree => identifier
  }

  def parseIndexedIdentifier(trees: this.~[AnnotatedTree, List[AnnotatedTree]]): AnnotatedTree = trees match {
    case symbol ~ indexes => AnnotatedTree.identifierOnly(Identifier((symbol +: indexes).map(_.root.literal).mkString("_")))
  }

  def parseSort(matched: AnnotatedTree ~ List[AnnotatedTree]): AnnotatedTree = matched match {
    case ident ~ sorts => AnnotatedTree.withoutAnnotations(ident.root, sorts)
  }

  def parseDatatype(definition: this.~[AnnotatedTree, List[AnnotatedTree]]): AnnotatedTree = definition match {
    case name ~ constructors =>
      AnnotatedTree.withoutAnnotations(Language.datatypeId, name +: constructors.map(c => {
        val funType = AnnotatedTree.withoutAnnotations(Language.mapTypeId, c.subtrees.map(_.root.annotation.get) :+ name)
        c.copy(root = c.root.copy(annotation = Some(funType)))
      }))
  }

  def parseSelectorDecl(trees: this.~[AnnotatedTree, AnnotatedTree]): AnnotatedTree = trees match {
    case identifier ~ sort => identifier.copy(root = identifier.root.copy(annotation = Some(sort)))
  }

  def parseConstructorDecl(trees: this.~[AnnotatedTree, List[AnnotatedTree]]): AnnotatedTree = trees match {
    case name ~ params =>
      AnnotatedTree.withoutAnnotations(name.root, params)
  }

  def parseQualIdent(trees: this.~[AnnotatedTree, AnnotatedTree]): AnnotatedTree = trees match {
    // page 27 in the pdf.
    /* Recall that every function symbolfis separately associated with one or more ranks,
    each specifying the sorts off’s arguments and result. To simplify sort checking, a
    function symbolin a term can be annotated with one of its result sorts σ.
    Such an annotated function symbolis aqualified identifierof the form(asfσ). */
    case ident ~ sort => throw new NotImplementedError()
  }

  def parseLet(matched: this.~[this.~[List[AnnotatedTree ~ AnnotatedTree], Elem], AnnotatedTree]): AnnotatedTree = matched match {
    case binded ~ _ ~ term => term.replaceDescendants(binded.map({ case a ~ b => (a, b) }))
  }

  def parseForall(matched: this.~[this.~[List[AnnotatedTree], Elem], AnnotatedTree]): AnnotatedTree = matched match {
    case params ~ _ ~ term => term.replaceDescendants(params.map({
      case a =>
        (a, AnnotatedTree.identifierOnly(identifierGenerator.next().copy(literal = "?" + a.root.literal, annotation = a.root.annotation)))
    }))
  }

  def parseExists(matched: this.~[this.~[List[AnnotatedTree], Elem], AnnotatedTree]): AnnotatedTree = matched match {
    case params ~ _ ~ term => term.replaceDescendants(params.map({
      case a =>
        (a, AnnotatedTree.identifierOnly(identifierGenerator.next().copy(annotation = a.root.annotation)))
    }))
  }

  def parseMatch(matched: AnnotatedTree ~ Elem ~ List[AnnotatedTree]): AnnotatedTree = {
    throw new NotImplementedError("Need to fix parameter scoping")
    matched match {
      case term ~ _ ~ guards => AnnotatedTree.withoutAnnotations(Language.matchId, term +: guards)
    }
  }

  def parseAssert(matched: AnnotatedTree): AnnotatedTree = {
    matched
  }

  def parseFunctionDecl(matched: AnnotatedTree ~ Elem ~ List[AnnotatedTree] ~ Elem ~ AnnotatedTree): AnnotatedTree = matched match {
    case name ~ _ ~ params ~ _ ~ res =>
      AnnotatedTree.withoutAnnotations(Language.functionDeclId, List(name.copy(root = name.root.copy(annotation = Some(
        AnnotatedTree.withoutAnnotations(Language.mapTypeId, params :+ res)
      )))))
  }

  def parseFunctionDef(matched: AnnotatedTree ~ Elem ~ List[AnnotatedTree] ~ Elem ~ AnnotatedTree ~ AnnotatedTree): List[AnnotatedTree] = matched match {
    case x @ name ~ _ ~ params ~ _ ~ res ~ term =>
      List(
        parseFunctionDecl(x._1),
        AnnotatedTree.withoutAnnotations(Language.letId, List(
          AnnotatedTree.withoutAnnotations(name.root, params),
          term
        )))
  }

  def parseProgram(matched: List[List[AnnotatedTree]] ~ AnnotatedTree): List[AnnotatedTree] = matched match {
    case statementss ~ conjecture =>
      val statements = statementss.flatten
      assert(conjecture.root.literal == "not")
      assert(statements.forall(_.root.literal != "not"))
      statements :+ AnnotatedTree.withoutAnnotations(Language.assertId, List(conjecture.subtrees.head))
  }
}
