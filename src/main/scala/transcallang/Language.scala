package transcallang

import scala.language.implicitConversions
import scala.util.matching.Regex

object Language {

  // TODO: maybe use this
  object Annotations extends Enumeration {
    protected case class Val(anno: Regex) extends super.Val {}

    implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]

    val definition = Val("++".r)
    val limitSearch   = Val("lim\\([1-9][0-9]*\\)".r)
  }

  val applyLiteral: String ="@"
  val typeLiteral: String ="type"
  val mapTypeLiteral: String =":>"
  val innerTypeLiteral: String ="polymorphic"
  val lambdaLiteral: String ="↦"
  val splitLiteral: String ="/"
  val idLiteral: String ="id"
  val trueLiteral: String ="⊤"
  val falseLiteral: String ="⊥"
  val nilLiteral: String ="⟨⟩"
  val negLiteral: String ="¬"
  val holeLiteral: String ="_"
  val guardedLiteral: String ="⇒"
  val tacticLiteral: String ="->"
  val tupleLiteral: String ="(,)"
  val annotationLiteral: String ="Annotation"
  val letLiteral: String = "="
  val directedLetLiteral: String = ">>"
  val commandLiteral: String = "Command"
  val semicolonLiteral: String = ";"
  val typeBuilderLiteral: String = ":"
  val trueCondBuilderLiteral: String = "||>"
  val andCondBuilderLiteral: String = "|||"
  val consLiteral: String = "::"
  val setLiteral: String = "{.}"
  val stringLiteralLiteral: String = "\""
  val matchLiteral: String = "match"

  val applyId: Identifier = Identifier(applyLiteral)
  val typeId: Identifier = Identifier(typeLiteral)
  val mapTypeId: Identifier = Identifier(mapTypeLiteral)
  val innerTypeId: Identifier = Identifier(innerTypeLiteral)
  val lambdaId: Identifier = Identifier(lambdaLiteral)
  val splitId: Identifier = Identifier(splitLiteral)
  val idId: Identifier = Identifier(idLiteral)
  val trueId: Identifier = Identifier(trueLiteral)
  val falseId: Identifier = Identifier(falseLiteral)
  val nilId: Identifier = Identifier(nilLiteral)
  val negId: Identifier = Identifier(negLiteral)
  val holeId: Identifier = Identifier(holeLiteral)
  val guardedId: Identifier = Identifier(guardedLiteral)
  val tacticId: Identifier = Identifier(tacticLiteral)
  val tupleId: Identifier = Identifier(tupleLiteral)
  val annotationId: Identifier = Identifier(annotationLiteral)
  val letId: Identifier = Identifier(letLiteral)
  val directedLetId: Identifier = Identifier(directedLetLiteral)
  val commandId: Identifier = Identifier(commandLiteral)
  val semicolonId: Identifier = Identifier(semicolonLiteral)
  val typeBuilderId: Identifier = Identifier(typeBuilderLiteral)
  val trueCondBuilderId: Identifier = Identifier(trueCondBuilderLiteral)
  val andCondBuilderId: Identifier = Identifier(andCondBuilderLiteral)
  val consId: Identifier = Identifier(consLiteral)
  val setId: Identifier = Identifier(setLiteral)
  val stringLiteralId: Identifier = Identifier(stringLiteralLiteral)
  val matchId: Identifier = Identifier(matchLiteral)

  val identifierRegex: Regex = "[?]?[\\w'`_]+".r

  val builtinConsts: Seq[String] = Seq("⟨⟩", "true", "false", "⊤", "⊥")
  val builtinNotOps: Seq[String] = Seq("~", "¬")
  val builtinSetArithOps: Seq[String] = Seq("++", "+", "-", "∪")
  val builtinSetBuildingOps: Seq[String] = Seq(":+", "::")
  val builtinAndOps: Seq[String] = Seq("/\\", "∧")
  val builtinOrOps: Seq[String] = Seq("\\/", "∨")
  val builtinIFFOps: Seq[String] = Seq("<->")
  val builtinBooleanOps: Seq[String] = Seq("==", "≠", "!=", "∈", "∉", "‖", "<", ">", "<=", ">=", "≤", "≥")
  val builtinCondBuilders: Seq[String] = Seq("||>")
  val builtinHighLevel: Seq[String] = Seq("/", "=>", "↦", "⇒", "|||")
  val builtinDefinitions: Seq[String] = Seq(letLiteral, directedLetLiteral)

  val builtinCommands: Seq[String] = Seq("→", "←", "[]", "□", "->", "<-")

  val arity: Map[String, Int] =
    (builtinBooleanOps ++ builtinSetArithOps ++ builtinSetArithOps).zip(Stream continually 2).toMap ++
      builtinNotOps.zip(Stream continually 1).toMap ++
      builtinConsts.zip(Stream continually 0).toMap


  val listId = Identifier("list")
  val intId = Identifier("int")
//  def getIdentfier(text: String): Identifier = textToIdentifier.getOrElse(text, Identifier(text))
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
