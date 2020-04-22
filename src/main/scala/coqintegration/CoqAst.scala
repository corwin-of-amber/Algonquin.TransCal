package coqintegration

import transcallang.Identifier

case class CoqAst(root: Identifier, expressionType: Option[CoqAst], subtrees: Seq[CoqAst]) {

}

object CoqAst {
  import play.api.libs.json._

  def readJson(input: String): Seq[CoqAst] = {
    // TODO: Also return list of types for each var\function
    val json = Json.parse(input) match {
      case x: JsArray => x
      case _ => throw new IllegalArgumentException("Argument should be a list of terms. Did not receive json list.")
    }

    def errorRaising[T](): T =  throw new IllegalArgumentException("Argument structure not correct. Expecting a __ / def /term and __/ def / types")
    json.value.map {
      case (obj: JsObject) if obj.keys.contains("def") =>
        val defObj = obj.value("def").validate[JsObject].getOrElse(errorRaising())
        fromJsObject(
          defObj.value("term").validate[JsObject].getOrElse(errorRaising()),
          defObj.value("types").validate[JsArray].getOrElse(errorRaising())
          .value.map(js => fromJsObject(js.validate[JsObject].get, Seq.empty))
        )
      case _ => errorRaising()
    }
  }

  def fromJsObject(obj: JsObject, types: Seq[CoqAst]): CoqAst = {
    val expType = types(obj("type_index").validate[JsNumber].get.value.toInt)
    val termJson = obj("term").validate[JsArray].get
    val termLiteral = termJson(0).validate[JsString].get.value
    termLiteral match {
      case "Lambda" =>
        val paramName = termJson(1).validate[JsObject].get("binder_name") // Goto binder name object
          .validate[JsArray].get.value(1) // go to id part (expecting index 0 to be "Name")
          .validate[JsArray].get.value(1) // go to value (expecting index 0 to be "Id")
          .validate[JsString].get.value
        // val paramType = 
    }
    CoqAst(Identifier(""), None, Seq.empty)
  }
}
