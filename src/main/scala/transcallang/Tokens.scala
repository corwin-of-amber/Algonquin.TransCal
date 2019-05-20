package transcallang

import scala.util.parsing.input.Positional

object Tokens {

  sealed trait WorkflowToken extends Positional {
    def toIdentifier: Identifier
  }

  private def I(value: String) = Identifier(value)
  private def I(value: Int) = Identifier(value.toString)

  case class IDENTIFIER(str: String) extends WorkflowToken {
    override val toIdentifier: Identifier = Identifier(str)
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
    override val toIdentifier: Identifier = Language.matchId
  }

  case class COLON() extends WorkflowToken {override val toIdentifier: Identifier = I(":")} // :
  case class DOUBLECOLON() extends WorkflowToken {override val toIdentifier: Identifier = Language.consId} // ::
  case class LAMBDA() extends WorkflowToken {override val toIdentifier: Identifier = Language.lambdaId } // ↦
  case class LET() extends WorkflowToken {override val toIdentifier: Identifier = Language.letId } // =
  case class DIRECTEDLET() extends WorkflowToken {override val toIdentifier: Identifier = Language.directedLetId} // >>
  case class LIMITEDLET() extends WorkflowToken {override val toIdentifier: Identifier = Language.limitedLetId} // |=
  case class LIMITEDDIRECTEDLET() extends WorkflowToken {override val toIdentifier: Identifier = Language.limitedDirectedLetId} // |>>
  case class EQUALS() extends WorkflowToken {override val toIdentifier: Identifier = Language.equalityId} // ==
  case class COMMA() extends WorkflowToken {override val toIdentifier: Identifier = I(",")} // ,
  case class TYPE() extends WorkflowToken{override val toIdentifier: Identifier = I("type")}

  case class MAPTYPE() extends WorkflowToken {override val toIdentifier: Identifier = I(":>")} // :>
  case class POLYMORPHIC() extends WorkflowToken {override val toIdentifier: Identifier = I("polymorphic")}

  case class BACKSLASH() extends WorkflowToken {override val toIdentifier: Identifier = Language.splitId} // /

  case class FALSE() extends WorkflowToken {override val toIdentifier: Identifier = Language.falseId} // ⊥
  case class TRUE() extends WorkflowToken {override val toIdentifier: Identifier = Language.trueId} // ⊤
  case class NIL() extends WorkflowToken {override val toIdentifier: Identifier = Language.nilId} // ⟨⟩
  case class NOT() extends WorkflowToken {override val toIdentifier: Identifier = Language.negId} // ¬
  case class GUARDED() extends WorkflowToken {override val toIdentifier: Identifier = Language.guardedId} // =>
  case class RIGHTARROW() extends WorkflowToken {override val toIdentifier: Identifier = Language.tacticId} // ->
  case class LEFTARROW() extends WorkflowToken {override val toIdentifier: Identifier = I("<-")} // <-
  case class HOLE() extends WorkflowToken {override val toIdentifier: Identifier = Language.holeId} // _
  case class SEMICOLON() extends WorkflowToken {override val toIdentifier: Identifier = Language.semicolonId} // ;
  case class TRUECONDBUILDER() extends WorkflowToken {override val toIdentifier: Identifier = Language.trueCondBuilderId} // ||>
  case class ANDCONDBUILDER() extends WorkflowToken {override val toIdentifier: Identifier = Language.andCondBuilderId} // |||
  case class ROUNDBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("(")} // (
  case class ROUNDBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I(")")} // )
  case class CURLYBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("{")} // {
  case class CURLYBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I("}")} // }
  case class SQUAREBRACETOPEN() extends WorkflowToken {override val toIdentifier: Identifier = I("[")} // [
  case class SQUAREBRACETCLOSE() extends WorkflowToken {override val toIdentifier: Identifier = I("]")} // ]
  case class SQUARE() extends WorkflowToken {override val toIdentifier: Identifier = I("□")} // "□"
  case class SNOC() extends WorkflowToken {override val toIdentifier: Identifier = Language.snocId} // ":+"
  case class PLUSPLUS() extends WorkflowToken {override val toIdentifier: Identifier = Language.concatId} // "++"
  case class PLUS() extends WorkflowToken {override val toIdentifier: Identifier = Language.plusId} // "+"
  case class MINUS() extends WorkflowToken {override val toIdentifier: Identifier = Language.minusId} // "-"
  case class UNION() extends WorkflowToken {override val toIdentifier: Identifier = Language.unionId} // "∪"
  case class NOTEQUALS() extends WorkflowToken {override val toIdentifier: Identifier = Language.unequalityId} // ("≠" | "!=")
  case class SETIN() extends WorkflowToken {override val toIdentifier: Identifier = Language.setContainsId} // "∈"
  case class SETNOTIN() extends WorkflowToken {override val toIdentifier: Identifier = Language.setNotContainsId} // "∉"
  case class SETDISJOINT() extends WorkflowToken {override val toIdentifier: Identifier = Language.setDisjointId} // "‖"
  case class LT() extends WorkflowToken {override val toIdentifier: Identifier = Language.ltId} // "<"
  case class LE() extends WorkflowToken {override val toIdentifier: Identifier = Language.leId} // ("<=" | "≤")
  case class GE() extends WorkflowToken {override val toIdentifier: Identifier = Language.geId} // (">=" | "≥")
  case class GT() extends WorkflowToken {override val toIdentifier: Identifier = Language.gtId} // ">"
  case class AND() extends WorkflowToken {override val toIdentifier: Identifier = Language.andId} // ("/\\" | "∧")
  case class OR() extends WorkflowToken {override val toIdentifier: Identifier = Language.orId} // ("\\/" | "∨")

}
