package language

import syntax.Identifier

package object Language {
  val builtinConsts: Seq[String] = Seq("⟨⟩", "true", "false", "⊤", "⊥")
  val builtinNotOps: Seq[String] = Seq("~", "¬")
  val builtinSetArithOps: Seq[String] = Seq("++", "+", "-", "∪")
  val builtinSetBuildingOps: Seq[String] = Seq(":+", "::")
  val builtinAndOps: Seq[String] = Seq("/\\", "∧")
  val builtinOrOps: Seq[String] = Seq("\\/", "∨")
  val builtinIFFOps: Seq[String] = Seq("<->")
  val builtinBooleanOps: Seq[String] = Seq("=", "≠", "!=", "∈", "∉", ", , ", "‖", "<", ">", "<=", ">=", "≤", "≥")
  val builtinDragOps: Seq[String] = Seq(":", "/", "↦", "->", "=>")
  
  val builtinCommands: Seq[String] = Seq("->", "→", "<-", "←", "[]", "□")

  val arity: Map[String, Int] =
    (builtinBooleanOps ++ builtinSetArithOps ++ builtinSetArithOps).zip(Stream continually 2).toMap ++
      builtinNotOps.zip(Stream continually 1).toMap ++
      builtinConsts.zip(Stream continually 0).toMap

  val applyId: Identifier = new Identifier("@")
  val lambdaId: Identifier = new Identifier("↦")
  val splitId: Identifier = new Identifier("/")
  val idId: Identifier = new Identifier("id")
  val trueId: Identifier = new Identifier("⊤")
  val falseId: Identifier = new Identifier("⊥")
  val nilId: Identifier = new Identifier("⟨⟩")
  val negId: Identifier = new Identifier("¬")
  val holeId: Identifier = new Identifier("_")

//  def getIdentfier(text: String): Identifier = textToIdentifier.getOrElse(text, new Identifier(text))
//  private val textToIdentifier = Map(
//    "@" -> applyId,
//    "apply" -> applyId,
//    "↦" -> lambdaId,
//    "lambda" -> lambdaId,
//    "/" -> splitId,
//    "split" -> splitId,
//    "id" -> idId,
//    "true" -> trueId,
//    "⊤" -> trueId,
//    "false" -> falseId,
//    "⊥" -> falseId,
//    "Nil" -> nilId,
//    "⟨⟩" -> nilId,
//    "neg" -> negId,
//    "¬" -> negId,
//    "~" -> negId
//  )
}
