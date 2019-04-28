package transcallang


import transcallang.Tokens._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position

object Lexer extends RegexParsers {
  case class WorkflowLexerError(location: Position, msg: String)

  override def skipWhitespace = true
  override val whiteSpace = "[ \t]+".r

  def apply(code: String): Either[WorkflowLexerError, List[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(next.pos, msg))
      case Success(result, next) => Right(result)
    }
  }

  def tokens: Parser[List[WorkflowToken]] = {
    phrase(rep1(matchkeyword | polymorphickeyword | typekeyword | falsekeyword  | truekeyword | snoc |
      truecondbuilder | andcondbuilder | doublecolon | maptype | comma | equals | semicolon | colon | nil | not | guarded
      | notequals | setdisjoint | plusplus | rightarrow | le | ge | and | or | leftarrow
      | backslash | lambda | let | plus | minus | union | directedlet | hole | setin | setnotin | lt | gt | annotation
      | roundbracetopen | roundbracetclose | squarebracetopen | squarebracetclose | curlybracetopen | curlybracetclose | square
      | number | literal | identifier)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: List[WorkflowToken],
                                  indents: List[Int] = List(0)): List[WorkflowToken] = {
    tokens
  }

  def identifier: Parser[IDENTIFIER] = positioned {
    Language.identifierRegex ^^ { str => IDENTIFIER(str) }
  }

  def literal: Parser[LITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

//  def indentation: Parser[INDENTATION] = positioned {
//    "\n[ ]*".r ^^ { whitespace =>
//      val nSpaces = whitespace.length - 1
//      INDENTATION(nSpaces)
//    }
//  }

  def number: Parser[NUMBER] = positioned { "\\d+".r ^^ {x => NUMBER(x.toInt)}}

  def annotation: Parser[ANNOTATION] = positioned { "\\[.+?\\]".r ^^ ( x => ANNOTATION(x.substring(1, x.length - 1)) ) }

  def matchkeyword: Lexer.Parser[MATCH] = positioned { "match" ^^ (_ => MATCH() ) }
  def colon: Lexer.Parser[COLON] = positioned { ":" ^^ (_ => COLON() ) }
  def doublecolon: Lexer.Parser[DOUBLECOLON] = positioned { "::" ^^ (_ => DOUBLECOLON() ) }
  def lambda: Lexer.Parser[LAMBDA] = positioned { "↦" ^^ (_ => LAMBDA() ) }
  def let: Lexer.Parser[LET] = positioned { "=" ^^ (_ => LET() ) }
  def directedlet: Lexer.Parser[DIRECTEDLET] = positioned { ">>" ^^ (_ => DIRECTEDLET() ) }
  def equals: Lexer.Parser[EQUALS] = positioned { "==" ^^ (_ => EQUALS() ) }
  def comma: Lexer.Parser[COMMA] = positioned { "," ^^ (_ => COMMA() ) }
  def typekeyword: Lexer.Parser[TYPE] = positioned { "type" ^^ (_ => TYPE() ) }
  def maptype: Lexer.Parser[MAPTYPE] = positioned { ":>" ^^ (_ => MAPTYPE() ) }
  def polymorphickeyword: Lexer.Parser[POLYMORPHIC] = positioned { "polymorphic" ^^ (_ => POLYMORPHIC() ) }
  def backslash: Lexer.Parser[BACKSLASH] = positioned { "/" ^^ (_ => BACKSLASH() ) }
  def falsekeyword: Parser[FALSE] = positioned { ("⊥" | "false") ^^ ( _ => FALSE() ) }
  def truekeyword: Parser[TRUE] = positioned { ("⊤" | "true") ^^ ( _ => TRUE() ) }
  def nil: Lexer.Parser[NIL] = positioned { ("⟨⟩" | "nil") ^^ (_ => NIL() ) }
  def not: Lexer.Parser[NOT] = positioned { ("¬" | "~") ^^ (_ => NOT() ) }
  def guarded: Lexer.Parser[GUARDED] = positioned { ("=>" | "⇒") ^^ (_ => GUARDED() ) }
  def rightarrow: Lexer.Parser[RIGHTARROW] = positioned { ("->"| "→") ^^ (_ => RIGHTARROW() ) }
  def leftarrow: Lexer.Parser[LEFTARROW] = positioned { ("<-" | "←") ^^ (_ => LEFTARROW())}
  def hole: Lexer.Parser[HOLE] = positioned { "_" ^^ (_ => HOLE() ) }
  def semicolon: Lexer.Parser[SEMICOLON] = positioned { (";" | "\n") ^^ (_ => SEMICOLON() ) }
  def truecondbuilder: Lexer.Parser[TRUECONDBUILDER] = positioned { "||>" ^^ (_ => TRUECONDBUILDER() ) }
  def andcondbuilder: Lexer.Parser[ANDCONDBUILDER] = positioned { "|||" ^^ (_ => ANDCONDBUILDER() ) }
  def roundbracetopen: Lexer.Parser[ROUNDBRACETOPEN] = positioned { "(" ^^ (_ => ROUNDBRACETOPEN() ) } // (
  def roundbracetclose: Lexer.Parser[ROUNDBRACETCLOSE] = positioned { ")" ^^ (_ => ROUNDBRACETCLOSE() ) } // )
  def curlybracetopen: Lexer.Parser[CURLYBRACETOPEN] = positioned { "{" ^^ (_ => CURLYBRACETOPEN() ) } // {
  def curlybracetclose: Lexer.Parser[CURLYBRACETCLOSE] = positioned { "}" ^^ (_ => CURLYBRACETCLOSE() ) } // }
  def squarebracetopen: Lexer.Parser[SQUAREBRACETOPEN] = positioned { "[" ^^ (_ => SQUAREBRACETOPEN() ) } // [
  def squarebracetclose: Lexer.Parser[SQUAREBRACETCLOSE] = positioned { "]" ^^ (_ => SQUAREBRACETCLOSE() ) } // ]
  def square: Lexer.Parser[SQUARE] = positioned { "□" ^^ (_ => SQUARE() ) } // □
  def snoc: Lexer.Parser[SNOC] = positioned { ":+" ^^ (_ => SNOC() ) } // :+
  def plusplus: Lexer.Parser[PLUSPLUS] = positioned { "++" ^^ ( _ => PLUSPLUS() ) }
  def plus: Lexer.Parser[PLUS] = positioned { "+" ^^ ( _ => PLUS() ) }
  def minus: Lexer.Parser[MINUS] = positioned { "-" ^^ ( _ => MINUS() ) }
  def union: Lexer.Parser[UNION] = positioned { "∪" ^^ ( _ => UNION() ) }
  def notequals: Lexer.Parser[NOTEQUALS] = positioned { ("≠" | "!=") ^^ ( _ => NOTEQUALS() ) }
  def setin: Lexer.Parser[SETIN] = positioned { "∈" ^^ ( _ => SETIN() ) }
  def setnotin: Lexer.Parser[SETNOTIN] = positioned { "∉" ^^ ( _ => SETNOTIN() ) }
  def setdisjoint: Lexer.Parser[SETDISJOINT] = positioned { "‖" ^^ ( _ => SETDISJOINT() ) }
  def lt: Lexer.Parser[LT] = positioned { "<" ^^ ( _ => LT() ) }
  def le: Lexer.Parser[LE] = positioned { ("<=" | "≤") ^^ ( _ => LE() ) }
  def ge: Lexer.Parser[GE] = positioned { (">=" | "≥") ^^ ( _ => GE() ) }
  def gt: Lexer.Parser[GT] = positioned { ">" ^^ ( _ => GT() ) }
  def and: Parser[AND] = positioned { ("/\\" | "∧") ^^ ( _ => AND() ) }
  def or: Parser[OR] = positioned { ("\\/" | "∨") ^^ ( _ => OR() ) }
}