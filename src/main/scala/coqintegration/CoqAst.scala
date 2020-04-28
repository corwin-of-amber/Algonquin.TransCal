package coqintegration

import jdk.jshell.spi.ExecutionControl.NotImplementedException
import play.api.libs
import transcallang.{Identifier, Language}
import play.api.libs.json._

case class CoqAst(root: Identifier, expressionType: Option[CoqAst], subtrees: Seq[CoqAst]) {

}

object CoqAst {
  val TypeAst = CoqAst(Language.typeId, None, Seq.empty)

  def parseId(arr: JsArray) = {
    assert((arr \ 0).validate[String].get == "Id")
    (arr \ 1).validate[String].get
  }

  def parseModPath(jsArray: JsArray) = {
    (jsArray \ 0).validate[JsString].get.value match {
      case "MPfile" =>
        val mpArr = (jsArray \ 1).validate[JsArray].get
        assert((mpArr \ 0).validate[JsString].get.value == "DirPath")
        (mpArr \ 1).validate[JsArray].get.value.map(js => parseId(js.validate[JsArray].get)).mkString(".")
      case "MPbound" =>
        // Apperantly this is a type of bound module that includes additional int and id.
        // No example of this yet.
        val index = (jsArray \ 0).validate[JsNumber].get.value.toInt
        val id = parseId((jsArray \ 1).validate[JsArray].get)
        val mpArr = (jsArray \ 2).validate[JsArray].get
        assert((mpArr \ 0).validate[JsString].get.value == "DirPath")
        id + index + "." + mpArr.value.tail.map(js => parseId(js.validate[JsArray].get)).mkString(".")
      case "MPdot" =>
        throw new IllegalArgumentException("I think this should be deprecated")
    }

  }

  def parseKernelName(jsArray: JsArray) = {
    // Kernel pair might include 2 names. Just to be sure verify size for now
    assert(jsArray.value.size == 3)
    val mp = parseModPath((jsArray \ 1).validate[JsArray].get)
    val id = parseId((jsArray \ 2).validate[JsArray].get)
    s"$mp.$id"
  }

  def fromJson(json: JsArray, types: Seq[CoqAst]): CoqAst = {
    val termLiteral = (json \ 0).validate[String]
    termLiteral.get match {
      case "Lambda" | "Prod" =>
        assert((json \ 1 \ "binder_name" \ 1 \ 0).validate[String].contains("Id"))
        val paramName = (json \ 1 \ "binder_name" // Goto binder name object
          \ 1 // go to id part (expecting index 0 to be "Name")
          \ 1) // go to value (expecting index 0 to be "Id")
          .validate[String].get
        val paramType = fromJson((json \ 2).get.validate[JsArray].get, types)
        val param = CoqAst(Identifier(paramName), Some(paramType), Seq.empty)
        val recRes = fromJson((json \ 3).validate[JsArray].get, types :+ param)
        termLiteral.get match {
          case "Lambda" =>
            CoqAst(transcallang.Language.lambdaId,
              // mapping exptype to save an unecessary if
              recRes.expressionType.map(t => CoqAst(Language.mapTypeId, None, Seq(paramType, t))),
              Seq(param, recRes))
          case "Prod" =>
            CoqAst(transcallang.Language.forallId,
              // mapping exptype to save an unecessary if
              Some(CoqAst.TypeAst),
              Seq(param, recRes))
        }
      case "App" =>
        // TODO: Add flatten to work **at the end** of translation. If it can be seperate it should.
        val funcTerm = fromJson((json \ 1).validate[JsArray].get, types)
        val paramTerms = (json \ 2).validate[JsArray].get.value
          .map(js => fromJson(js.validate[JsArray].get, types))
        val appType = funcTerm.expressionType.map(et => {
          assert(paramTerms.zipWithIndex.forall(t => t._1.expressionType.forall(pt => {
            et.subtrees(t._2) == pt
          })))
          if (et.subtrees.size == 2) et.subtrees.last
          else et.copy(subtrees = et.subtrees.drop(paramTerms.size))
        })
        CoqAst(Language.applyId, appType, funcTerm +: paramTerms)
      case "Sort" =>
        CoqAst.TypeAst
      case "Rel" =>
        // TODO: Check if irrelevant means should not insert to types
        types((json \ 1).validate[JsNumber].get.value.toInt - 1)
      case "Const" =>
        val const = (json \ 1)
        val constant = (const \ 0)
        // universe is in (const \ 1) but we ignore it as all universes for us is type.
        assert((constant \ 0).validate[JsString].get.value == "Constant")
        CoqAst(Identifier(parseKernelName(constant.validate[JsArray].get)), None, Seq.empty)
      case "Var" =>
        throw new NotImplementedError("Will implement once an example is found. Only need to use parseId.")
      case "Ind" =>
        // int * MutInd (MutInd = KerPair = KernalName)
        val ind = (json \ 1 \ 0)
        // Ignoring universe in json \ 1 \ 1
        parseInductive(ind.validate[JsArray].get)
      case "Int" =>
        throw new NotImplementedException("Implement once encountered an int example")
      case "Float" =>
        throw new NotImplementedException("Implement once encountered a float example")
      case "Construct" =>
        // This is an application of a constructor.
        // TODO: Keep which constructor for type by index
        assert((json \ 0).validate[String].get == "Construct")
        // ignore universe
        val constructor = (json \ 1 \ 0)
        val constIndex = (constructor \ 1)
        parseInductive((constructor \ 0).validate[JsArray].get)
      case "LetIn" =>
        throw new NotImplementedException("Implement once encountered a let example")
      case "Fix" =>
        throw new NotImplementedException("Implement once encountered a fixpoint example")
      case "CoFix" =>
        throw new NotImplementedException("We do not support mutual recursion yet")
    }
  }

  private def parseInductive(ind: JsArray) = {
    if ((ind \ 1).validate[JsNumber].get.value.toInt != 0)
      throw new NotImplementedException("We do not support mutual induction yet")
    val mutInd = (ind \ 0)
    assert((mutInd \ 0).validate[JsString].get.value == "MutInd")
    CoqAst(Identifier(parseKernelName(mutInd.validate[JsArray].get)), None, Seq.empty)
  }
}
