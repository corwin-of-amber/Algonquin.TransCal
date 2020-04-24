package transcallang

import scala.language.implicitConversions
import scala.util.matching.Regex

object Language {
  object Annotations extends Enumeration {
    protected case class Val(anno: Regex) extends super.Val {}

    implicit def valueToVal(x: Value): Val = x.asInstanceOf[Val]

    val definition = Val("++".r)
    val limitSearch   = Val("lim\\([1-9][0-9]*\\)".r)
  }

  val typeIntId = Identifier("int")
  val typeBooleanId = Identifier("boolean")
  val typeListId = Identifier("list")
  val typeSetId = Identifier("set")
  val typeId: Identifier = Identifier("type")
  val mapTypeId: Identifier = Identifier(":>")

  val typeBoolean: AnnotatedTree = AnnotatedTree.identifierOnly(typeBooleanId)
  val typeInt: AnnotatedTree = AnnotatedTree.identifierOnly(typeIntId)
  val typeBooleanToBoolean: AnnotatedTree = AnnotatedTree.withoutAnnotations(mapTypeId, Seq(typeBoolean, typeBoolean))
  val typeListInt: AnnotatedTree = AnnotatedTree.withoutAnnotations(typeListId, Seq(typeInt))
  val typeIntListIntToListInt: AnnotatedTree = AnnotatedTree.withoutAnnotations(mapTypeId, Seq(typeInt, typeListInt, typeListInt))
  val typeListIntIntToListInt: AnnotatedTree = AnnotatedTree.withoutAnnotations(mapTypeId, Seq(typeListInt, typeInt, typeListInt))

  val applyId: Identifier = Identifier("@")
  val innerTypeId: Identifier = Identifier("polymorphic")
  val lambdaId: Identifier = Identifier("↦")
  val splitId: Identifier = Identifier("/")
  val idId: Identifier = Identifier("id")
//  val trueId: Identifier = Identifier("⊤")
  val trueId: Identifier = Identifier("⊤", annotation=Some(typeBoolean))
//  val falseId: Identifier = Identifier("⊥")
  val falseId: Identifier = Identifier("⊥", annotation=Some(typeBoolean))
  val nilId: Identifier = Identifier("⟨⟩", annotation = Some(typeListInt))
//  val negId: Identifier = Identifier("¬")
  val negId: Identifier = Identifier("¬", annotation=Some(typeBooleanToBoolean))
  val holeId: Identifier = Identifier("_")
  val guardedId: Identifier = Identifier("⇒")
  val tacticId: Identifier = Identifier("->")
  val tupleId: Identifier = Identifier("(,)")
  val annotationId: Identifier = Identifier("Annotation")
  val letId: Identifier = Identifier("=")
  val directedLetId: Identifier = Identifier(">>")
  val limitedLetId: Identifier = Identifier("|=")
  val limitedDirectedLetId: Identifier = Identifier("|>>")
  val commandId: Identifier = Identifier("Command")
  val semicolonId: Identifier = Identifier(";")
  val typeBuilderId: Identifier = Identifier(":")
  val trueCondBuilderId: Identifier = Identifier("||>")
  val andCondBuilderId: Identifier = Identifier("|||")
  val limitedAndCondBuilderId: Identifier = Identifier("||||")
  val consId: Identifier = Identifier("::", annotation = Some(typeIntListIntToListInt))
  val snocId: Identifier = Identifier(":+", annotation = Some(typeListIntIntToListInt))
  val setId: Identifier = Identifier("{.}")
  val stringLiteralId: Identifier = Identifier("\"")
  val matchId: Identifier = Identifier("match")
  val equalityId: Identifier = Identifier("==")
  val unequalityId : Identifier = Identifier("≠")
  val setContainsId: Identifier = Identifier("∈")
  val setNotContainsId: Identifier = Identifier("∉")
  val setDisjointId: Identifier = Identifier("‖")
  val ltId: Identifier = Identifier("<")
  val gtId: Identifier = Identifier(">")
  val leId: Identifier = Identifier("≤")
  val geId: Identifier = Identifier("≥")
  val concatId: Identifier = Identifier("++")
  val plusId: Identifier = Identifier("+")
  val minusId: Identifier = Identifier("-")
  val unionId: Identifier = Identifier("∪")
  val andId: Identifier = Identifier("∧")
  val orId: Identifier = Identifier("∨")
  val timeComplexId = Identifier("timecomplex")
  val timeComplexTrueId = Identifier("timecomplexTrue")
  val typeTrueId = Identifier("typeTrue")
  val spbeId = Identifier("SPBE")
  val forallId = Identifier("forall")

  val identifierRegex: Regex = "[?]?[\\w'`_]+".r

  val builtinConsts: Seq[Identifier] = Seq(nilId, trueId, falseId)
  val builtinNotOps: Seq[Identifier] = Seq(negId)
  val builtinSetArithOps: Seq[Identifier] = Seq(concatId, plusId, minusId, unionId)
  val builtinSetBuildingOps: Seq[Identifier] = Seq(snocId, consId)
  val builtinBooleanOps: Seq[Identifier] = Seq(equalityId, unequalityId, setContainsId, setNotContainsId, setDisjointId, ltId, gtId, leId, geId)
  val builtinCondBuilders: Seq[Identifier] = Seq(trueCondBuilderId)
  val builtinHighLevel: Seq[Identifier] = Seq(splitId, guardedId, lambdaId, andCondBuilderId, limitedAndCondBuilderId)
  val builtinLimitedDefinitions: Seq[Identifier] = Seq(limitedLetId, limitedDirectedLetId)
  val builtinDirectedDefinitions: Seq[Identifier] = Seq(directedLetId, limitedDirectedLetId)
  val builtinDefinitions: Seq[Identifier] = Seq(letId, limitedLetId) ++ builtinDirectedDefinitions

  val builtinCommands: Seq[String] = Seq("→", "←", "[]", "□", "->", "<-")

  val arity: Map[String, Int] =
    (builtinBooleanOps ++ builtinSetArithOps ++ builtinSetArithOps).map(_.literal).zip(Stream continually 2).toMap ++
      builtinNotOps.map(_.literal).zip(Stream continually 1).toMap ++
      builtinConsts.map(_.literal).zip(Stream continually 0).toMap

}
