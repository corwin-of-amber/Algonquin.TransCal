package transcallang

import syntax.Identifier

import scala.util.parsing.input.Positional

object Tokens {

  sealed trait WorkflowToken extends Positional {
    def toIdentifier: Identifier
  }

  private def I(value: String) = new Identifier(value)
  private def I(value: Int) = new Identifier(value)

  case class IDENTIFIER(str: String) extends WorkflowToken {
    override val toIdentifier: Identifier = new Identifier(str)
  }

  case class LITERAL(str: String) extends WorkflowToken {
    override val toIdentifier: Identifier = I(str)
  }

  //  case class INDENTATION(spaces: Int) extends WorkflowToken
  case class ANNOTATION(str: String) extends WorkflowToken {
    override val toIdentifier: Identifier = I(str)
  }

  case class NUMBER(int: Int) extends WorkflowToken {
    override val toIdentifier: Identifier = I(int)
  }

  case class MATCH() extends WorkflowToken {
    override val toIdentifier: Identifier = I("match")
  }

  case class COLON() extends WorkflowToken {override val toIdentifier: Identifier = I(":")} // :
  case class DOUBLECOLON() extends WorkflowToken {override val toIdentifier: Identifier = I("::")} // ::
  case class LAMBDA() extends WorkflowToken {override val toIdentifier: Identifier = Language.lambdaId } // ↦
  case class LET() extends WorkflowToken {override val toIdentifier: Identifier = Language.letId } // =
  case class DIRECTEDLET() extends WorkflowToken {override val toIdentifier: Identifier = I(">>")} // >>
  case class EQUALS() extends WorkflowToken {override val toIdentifier: Identifier = I("==")} // ==
  case class COMMA() extends WorkflowToken {override val toIdentifier: Identifier = I(",")} // ,
  case class TYPE() extends WorkflowToken{override val toIdentifier: Identifier = I("type")}

  case class MAPTYPE() extends WorkflowToken {override val toIdentifier: Identifier = I(":>")} // :>
  case class POLYMORPHIC() extends WorkflowToken {override val toIdentifier: Identifier = I("polymorphic")}

  case class BACKSLASH() extends WorkflowToken {override val toIdentifier: Identifier = I("/")} // /

  case class FALSE() extends WorkflowToken {override val toIdentifier: Identifier = I("⊥")} // ⊥
  case class TRUE() extends WorkflowToken {override val toIdentifier: Identifier = I("⊤")} // ⊤
  case class NIL() extends WorkflowToken {override val toIdentifier: Identifier = I("⟨⟩")} // ⟨⟩
  case class NOT() extends WorkflowToken {override val toIdentifier: Identifier = I("¬")} // ¬
  case class GUARDED() extends WorkflowToken {override val toIdentifier: Identifier = I("=>")} // =>
  case class RIGHTARROW() extends WorkflowToken {override val toIdentifier: Identifier = I("->")} // ->
  case class LEFTARROW() extends WorkflowToken {override val toIdentifier: Identifier = I("<-")} // <-
  case class HOLE() extends WorkflowToken {override val toIdentifier: Identifier = I("_")} // _
  case class SEMICOLON() extends WorkflowToken {override val toIdentifier: Identifier = I(";")} // ;
  case class TRUECONDBUILDER() extends WorkflowToken {override val toIdentifier: Identifier = I("||>")} // ||>
  case class ANDCONDBUILDER() extends WorkflowToken {override val toIdentifier: Identifier = I("|||")} // |||
  case class ROUNDBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("(")} // (
  case class ROUNDBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I(")")} // )
  case class CURLYBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("{")} // {
  case class CURLYBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I("}")} // }
  case class SQUAREBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("[")} // [
  case class SQUAREBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I("]")} // ]
  case class SQUARE() extends WorkflowToken {override val toIdentifier: Identifier = I("□")} // "□"
  case class SNOC() extends WorkflowToken {override val toIdentifier: Identifier = I(":+")} // ":+"
  case class PLUSPLUS() extends WorkflowToken {override val toIdentifier: Identifier = I("++")} // "++"
  case class PLUS() extends WorkflowToken {override val toIdentifier: Identifier = I("+")} // "+"
  case class MINUS() extends WorkflowToken {override val toIdentifier: Identifier = I("-")} // "-"
  case class UNION() extends WorkflowToken {override val toIdentifier: Identifier = I("∪")} // "∪"
  case class NOTEQUALS() extends WorkflowToken {override val toIdentifier: Identifier = I("≠")} // ("≠" | "!=")
  case class SETIN() extends WorkflowToken {override val toIdentifier: Identifier = I("∈")} // "∈"
  case class SETNOTIN() extends WorkflowToken {override val toIdentifier: Identifier = I("∉")} // "∉"
  case class SETDISJOINT() extends WorkflowToken {override val toIdentifier: Identifier = I("‖")} // "‖"
  case class LT() extends WorkflowToken {override val toIdentifier: Identifier = I("<")} // "<"
  case class LE() extends WorkflowToken {override val toIdentifier: Identifier = I("≤")} // ("<=" | "≤")
  case class GE() extends WorkflowToken {override val toIdentifier: Identifier = I("≥")} // (">=" | "≥")
  case class GT() extends WorkflowToken {override val toIdentifier: Identifier = I(">")} // ">"
  case class AND() extends WorkflowToken {override val toIdentifier: Identifier = I("∧")} // ("/\\" | "∧")
  case class OR() extends WorkflowToken {override val toIdentifier: Identifier = I("∨")} // ("\\/" | "∨")

}
