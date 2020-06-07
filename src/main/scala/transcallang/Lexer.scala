package transcallang


import transcallang.Tokens._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Position

object Lexer extends RegexParsers {
  case class WorkflowLexerError(location: Position, msg: String)

//  override def skipWhitespace: Boolean = true
  protected override val whiteSpace: Regex = "[ \t]+".r

  def apply(code: String): Either[WorkflowLexerError, Seq[WorkflowToken]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, next) => Left(WorkflowLexerError(next.pos, msg))
      case Success(result, _) => Right(result)
    }
  }

  // Tokens is the main part of the lexer. Add your new token here!!!
  def tokens: Parser[Seq[WorkflowToken]] = {
    phrase(rep1(varargs | matchkeyword | polymorphickeyword | typekeyword | falsekeyword  | truekeyword | snoc |
      truecondbuilder | limitedandcondbuilder | andcondbuilder | limitedlet | limiteddirectedlet | doublecolon | maptype | comma | equals | semicolon | colon | nil | not | guarded
      | notequals | setdisjoint | plusplus | rightarrow | le | ge | and | or | leftarrow
      | backslash | lambda | let | plus | minus | union | directedlet | hole | setin | setnotin | lt | gt | annotation
      | roundbracetopen | roundbracetclose | squarebracetopen | squarebracetclose | curlybracetopen | curlybracetclose | square
      | number | literal | identifier)) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  private def processIndentations(tokens: Seq[WorkflowToken], indents: Seq[Int] = List(0)): Seq[WorkflowToken] = tokens

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

  // TODO: merge these with the token themselves
  def number: Parser[NUMBER] = positioned { "\\d+".r ^^ {x => NUMBER(x.toInt)}}

  def annotation: Parser[ANNOTATION] = positioned { "\\[.+?\\]".r ^^ ( x => ANNOTATION(x.substring(1, x.length - 1)) ) }

  def matchkeyword: Parser[MATCH] = positioned { "match" ^^ (_ => MATCH() ) }
  def colon: Parser[COLON] = positioned { ":" ^^ (_ => COLON() ) }
  def doublecolon: Parser[DOUBLECOLON] = positioned { "::" ^^ (_ => DOUBLECOLON() ) }
  def lambda: Parser[LAMBDA] = positioned { "↦" ^^ (_ => LAMBDA() ) }
  def let: Parser[LET] = positioned { "=" ^^ (_ => LET() ) }
  def directedlet: Parser[DIRECTEDLET] = positioned { ">>" ^^ (_ => DIRECTEDLET() ) }
  def limitedlet: Parser[LIMITEDLET] = positioned { "|=" ^^ (_ => LIMITEDLET() ) }
  def limiteddirectedlet: Parser[LIMITEDDIRECTEDLET] = positioned { "|>>" ^^ (_ => LIMITEDDIRECTEDLET() ) }
  def equals: Parser[EQUALS] = positioned { "==" ^^ (_ => EQUALS() ) }
  def comma: Parser[COMMA] = positioned { "," ^^ (_ => COMMA() ) }
  def typekeyword: Parser[TYPE] = positioned { "type" ^^ (_ => TYPE() ) }
  def maptype: Parser[MAPTYPE] = positioned { ":>" ^^ (_ => MAPTYPE() ) }
  def polymorphickeyword: Parser[POLYMORPHIC] = positioned { "polymorphic" ^^ (_ => POLYMORPHIC() ) }

  def backslash: Parser[BACKSLASH] = positioned { "/" ^^ (_ => BACKSLASH() ) }
  def varargs: Parser[VARARGS] = positioned { Language.varargsId.literal ^^ (_ => VARARGS())}

  def falsekeyword: Parser[FALSE] = positioned { ("⊥" | "false") ^^ ( _ => FALSE() ) }
  def truekeyword: Parser[TRUE] = positioned { ("⊤" | "true") ^^ ( _ => TRUE() ) }
  def nil: Parser[NIL] = positioned { ("⟨⟩" | "nil") ^^ (_ => NIL() ) }
  def not: Parser[NOT] = positioned { ("¬" | "~") ^^ (_ => NOT() ) }
  def guarded: Parser[GUARDED] = positioned { ("=>" | "⇒") ^^ (_ => GUARDED() ) }
  def rightarrow: Parser[RIGHTARROW] = positioned { ("->"| "→") ^^ (_ => RIGHTARROW() ) }
  def leftarrow: Parser[LEFTARROW] = positioned { ("<-" | "←") ^^ (_ => LEFTARROW())}
  def hole: Parser[HOLE] = positioned { "_" ^^ (_ => HOLE() ) }
  def semicolon: Parser[SEMICOLON] = positioned { (";" | "\n") ^^ (_ => SEMICOLON() ) }
  def truecondbuilder: Parser[TRUECONDBUILDER] = positioned { "||>" ^^ (_ => TRUECONDBUILDER() ) }
  def andcondbuilder: Parser[ANDCONDBUILDER] = positioned { "|||" ^^ (_ => ANDCONDBUILDER() ) }
  def limitedandcondbuilder: Parser[LIMITEDANDCONDBUILDER] = positioned { "||||" ^^ (_ => LIMITEDANDCONDBUILDER() ) }
  def roundbracetopen: Parser[ROUNDBRACETOPEN] = positioned { "(" ^^ (_ => ROUNDBRACETOPEN() ) } // (
  def roundbracetclose: Parser[ROUNDBRACETCLOSE] = positioned { ")" ^^ (_ => ROUNDBRACETCLOSE() ) } // )
  def curlybracetopen: Parser[CURLYBRACETOPEN] = positioned { "{" ^^ (_ => CURLYBRACETOPEN() ) } // {
  def curlybracetclose: Parser[CURLYBRACETCLOSE] = positioned { "}" ^^ (_ => CURLYBRACETCLOSE() ) } // }
  def squarebracetopen: Parser[SQUAREBRACETOPEN] = positioned { "[" ^^ (_ => SQUAREBRACETOPEN() ) } // [
  def squarebracetclose: Parser[SQUAREBRACETCLOSE] = positioned { "]" ^^ (_ => SQUAREBRACETCLOSE() ) } // ]
  def square: Parser[SQUARE] = positioned { "□" ^^ (_ => SQUARE() ) } // □
  def snoc: Parser[SNOC] = positioned { ":+" ^^ (_ => SNOC() ) } // :+
  def plusplus: Parser[PLUSPLUS] = positioned { "++" ^^ ( _ => PLUSPLUS() ) }
  def plus: Parser[PLUS] = positioned { "+" ^^ ( _ => PLUS() ) }
  def minus: Parser[MINUS] = positioned { "-" ^^ ( _ => MINUS() ) }
  def union: Parser[UNION] = positioned { "∪" ^^ ( _ => UNION() ) }
  def notequals: Parser[NOTEQUALS] = positioned { ("≠" | "!=") ^^ ( _ => NOTEQUALS() ) }
  def setin: Parser[SETIN] = positioned { "∈" ^^ ( _ => SETIN() ) }
  def setnotin: Parser[SETNOTIN] = positioned { "∉" ^^ ( _ => SETNOTIN() ) }
  def setdisjoint: Parser[SETDISJOINT] = positioned { "‖" ^^ ( _ => SETDISJOINT() ) }
  def lt: Parser[LT] = positioned { "<" ^^ ( _ => LT() ) }
  def le: Parser[LE] = positioned { ("<=" | "≤") ^^ ( _ => LE() ) }
  def ge: Parser[GE] = positioned { (">=" | "≥") ^^ ( _ => GE() ) }
  def gt: Parser[GT] = positioned { ">" ^^ ( _ => GT() ) }
  def and: Parser[AND] = positioned { ("/\\" | "∧") ^^ ( _ => AND() ) }
  def or: Parser[OR] = positioned { ("\\/" | "∨") ^^ ( _ => OR() ) }
}