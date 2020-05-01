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

  /**
    *
    * @param obj representing binder definition
    * @return name, relevance
    */
  def parseBinder(obj: JsObject): (String, Boolean) = {
    val toAssert = Seq("Name", "Anonymous").contains((obj \ "binder_name" \ 0).validate[String].get)
    assert(toAssert)
    val paramName = (obj \ "binder_name" \ 0).validate[String].map({
      case "Name" => (obj \ "binder_name" // Goto binder name object
        \ 1 // go to id part (expecting index 0 to be "Name")
        \ 1).validate[String].get
      case "Anonymous" => "_"
    }).get
    val paramRelevance = (obj \ "binder_relevance" \ 0).validate[String].contains("Relevant")
    (paramName, paramRelevance)
  }

  def fromJson(json: JsArray, bound: Seq[CoqAst]): CoqAst = {
    val termLiteral = (json \ 0).validate[String]
    val translated = termLiteral.get match {
      case "Lambda" | "Prod" =>
        val (paramName, paramRelevance) = parseBinder((json \ 1).validate[JsObject].get)
        val paramType = fromJson((json \ 2).get.validate[JsArray].get, bound)
        val param = CoqAst(Identifier(paramName), Some(paramType), Seq.empty)
        val recRes = fromJson((json \ 3).validate[JsArray].get, param +: bound)
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
        // I am flattening here because at this point in time we know if a parameter is bound.
        // The only situation where flatten isn't feasable is when the funcTerm root is a parameter,
        // meaning it is bound.
        val funcTerm = fromJson((json \ 1).validate[JsArray].get, bound)
        val paramTerms = (json \ 2).validate[JsArray].get.value
          .map(js => fromJson(js.validate[JsArray].get, bound))
        val appType = funcTerm.expressionType.map(et => {
          assert(paramTerms.zipWithIndex.forall(t => t._1.expressionType.forall(pt => {
            et.subtrees(t._2) == pt
          })))
          if (et.subtrees.size == 2) et.subtrees.last
          else et.copy(subtrees = et.subtrees.drop(paramTerms.size))
        })
        val toFlatten = (json \ 1 \ 0).validate[String].get != "Rel"
        if (toFlatten) CoqAst(funcTerm.root, appType, funcTerm.subtrees ++ paramTerms)
        else CoqAst(Language.applyId, appType, funcTerm +: paramTerms)
      case "Sort" =>
        CoqAst.TypeAst
      case "Rel" =>
        // TODO: Check if irrelevant means should not insert to types
        // TODO: Can there be two parameters with the same name? Does let take care of this?
        bound((json \ 1).validate[JsNumber].get.value.toInt - 1)
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
        assert((json \ 0).validate[String].get == "Construct")
        // ignore universe
        val constructor = (json \ 1 \ 0)
        val constIndex = (constructor \ 1).get
        val typeAst = parseInductive((constructor \ 0).validate[JsArray].get)
        assert(typeAst.subtrees.isEmpty)
        typeAst.copy(typeAst.root.copy(literal = typeAst.root.literal + s".constructor_$constIndex"))
      case "LetIn" =>
        throw new NotImplementedException("Implement once encountered a let example")
      case "Fix" =>
        // First assert there are no mutual fixpoints
        assert((json \ 1 \ 0 \ 0).validate[Seq[JsValue]].get.length == 1)
        // Now if I understand correctly, "The second component [int] tells which component of the block is
        //     returned" means that with only one recursion the second arg is always 0.
        assert((json \ 1 \ 0 \ 1).validate[Int].get == 0)
        val recursiveArgIndex = (json \ 1 \ 0 \ 0 \ 0).validate[Int].get
        val prec_declaration = (json \ 1 \ 1)
        val arrayOfFunNames = (prec_declaration \ 0)
        assert(arrayOfFunNames.validate[JsArray].get.value.length == 1)
        val (funName, funRelevance) = parseBinder((arrayOfFunNames \ 0).validate[JsObject].get)
        // TODO: maybe fun name should be in types as it is a binder?
        val recType = fromJson((prec_declaration \ 1 \ 0).validate[JsArray].get, bound)
        val recFun = fromJson((prec_declaration \ 2 \ 0).validate[JsArray].get, bound)
        assert(recFun.expressionType.contains(recType))
        recFun.copy(expressionType = Some(recType))
      case "CoFix" =>
        throw new NotImplementedException("We do not support mutual recursion yet")
      case "Case" =>
        // Case      of case_info * 'constr * 'constr * 'constr array
        /** Found doc about case, return of this doc is the format of json:
          *
          * [mkCase ci p c ac] stand for match [c] as [x] in [I args] return [p] with [ac]
          * presented as describe in [ci].
          *
          * (** Destructs a [match c as x in I args return P with ... |
          *
          * Ci(...yij...) => ti | ... end] (or [let (..y1i..) := c as x in I args
          * return P in t1], or [if c then t1 else t2])
          *
          * @return [(info,c,fun args x => P,[|...|fun yij => ti| ...|])]
          *         where [info] is pretty-printing information *)
          */
        val caseInfo =  (json \ 1)
        val matchedType = parseInductive((caseInfo \ "ci_ind").validate[JsArray].get)
        val numTypeParams = (caseInfo \ "ci_npar").validate[Int].get
        // There is something called number of declerations which is different
        // from number of args for each constructor. Something to do with let.
        // Ignoring this for now
        val numConstructorArgs = (caseInfo \ "ci_cstr_nargs").validate[JsArray].get.value
          .map(js => js.validate[Int].get)
        // TODO: Not sure how to use relevance. This means I need to insert things to types?
        // Probably means i should insert x
        val relevance = (caseInfo \ "ci_relevance" \ 0).validate[String].contains("Relevant")
        // In coq when matching an alias can be given to matched and the return type can be specified.
        // asReturn is a lambda with parameter with as named (similar to let) and the body is the return type.
        val matched = fromJson((json \ 3).validate[JsArray].get, bound)
        val asReturn = fromJson((json \ 2).validate[JsArray].get, bound)
        val cases = (json \ 4).validate[Seq[JsArray]].get.map(j => fromJson(j, matched +: bound))
        CoqAst(Language.matchId, Some(asReturn.subtrees.last), matched +: cases)
    }
   translated
  }

  private def parseInductive(ind: JsArray) = {
    if ((ind \ 1).validate[JsNumber].get.value.toInt != 0)
      throw new NotImplementedException("We do not support mutual induction yet")
    val mutInd = (ind \ 0)
    assert((mutInd \ 0).validate[JsString].get.value == "MutInd")
    CoqAst(Identifier(parseKernelName(mutInd.validate[JsArray].get)), None, Seq.empty)
  }
}
