package language

import syntax.Identifier

import scala.util.matching.Regex

package object Language {
  val applyLiteral: String ="@"
  val lambdaLiteral: String ="↦"
  val splitLiteral: String ="/"
  val idLiteral: String ="id"
  val trueLiteral: String ="⊤"
  val falseLiteral: String ="⊥"
  val nilLiteral: String ="⟨⟩"
  val negLiteral: String ="¬"
  val holeLiteral: String ="_"
  val guardedLiteral: String ="=>"
  val tacticLiteral: String ="->"
  val tupleLiteral: String ="(,)"
  val annotationLiteral: String ="Annotation"
  val letLiteral: String = "="
  val directedLetLiteral: String = ">>"
  val commandLiteral: String = "Command"
  val semicolonLiteral: String = ";"
  val trueCondBuilderLiteral: String = "||>"
  val andCondBuilderLiteral: String = "|||"
  val consLiteral: String = "::"
  val setLiteral: String = "{.}"

  val applyId: Identifier = new Identifier(applyLiteral)
  val lambdaId: Identifier = new Identifier(lambdaLiteral)
  val splitId: Identifier = new Identifier(splitLiteral)
  val idId: Identifier = new Identifier(idLiteral)
  val trueId: Identifier = new Identifier(trueLiteral)
  val falseId: Identifier = new Identifier(falseLiteral)
  val nilId: Identifier = new Identifier(nilLiteral)
  val negId: Identifier = new Identifier(negLiteral)
  val holeId: Identifier = new Identifier(holeLiteral)
  val guardedId: Identifier = new Identifier(guardedLiteral)
  val tacticId: Identifier = new Identifier(tacticLiteral)
  val tupleId: Identifier = new Identifier(tupleLiteral)
  val annotationId: Identifier = new Identifier(annotationLiteral)
  val letId: Identifier = new Identifier(letLiteral)
  val directedLetId: Identifier = new Identifier(directedLetLiteral)
  val commandId: Identifier = new Identifier(commandLiteral)
  val semicolonId: Identifier = new Identifier(semicolonLiteral)
  val trueCondBuilderId: Identifier = new Identifier(trueCondBuilderLiteral)
  val andCondBuilderId: Identifier = new Identifier(andCondBuilderLiteral)
  val consId: Identifier = new Identifier(consLiteral)
  val setId: Identifier = new Identifier(setLiteral)

  val identifierRegex: Regex = "[?]?[\\w'_]+".r

  val builtinConsts: Seq[String] = Seq("⟨⟩", "true", "false", "⊤", "⊥")
  val builtinNotOps: Seq[String] = Seq("~", "¬")
  val builtinSetArithOps: Seq[String] = Seq("++", "+", "-", "∪")
  val builtinSetBuildingOps: Seq[String] = Seq(":+", "::")
  val builtinAndOps: Seq[String] = Seq("/\\", "∧")
  val builtinOrOps: Seq[String] = Seq("\\/", "∨")
  val builtinIFFOps: Seq[String] = Seq("<->")
  val builtinBooleanOps: Seq[String] = Seq("==", "≠", "!=", "∈", "∉", ", , ", "‖", "<", ">", "<=", ">=", "≤", "≥")
  val builtinCondBuilders: Seq[String] = Seq("||>")
  val builtinHighLevel: Seq[String] = Seq("/", "=>", "↦", "⇒", "|||")
  val builtinDefinitions: Seq[String] = Seq(letLiteral, directedLetLiteral)

  val builtinCommands: Seq[String] = Seq("→", "←", "[]", "□", "->", "<-")

  val arity: Map[String, Int] =
    (builtinBooleanOps ++ builtinSetArithOps ++ builtinSetArithOps).zip(Stream continually 2).toMap ++
      builtinNotOps.zip(Stream continually 1).toMap ++
      builtinConsts.zip(Stream continually 0).toMap

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
