package coqintegration

import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString}
import transcallang.Identifier

case class Environment(definitions: Seq[Definition]) {
  def this(json: JsArray) = this(
    // For each definition save its name and create a coq ast instance
    json.value.map {
      case (obj: JsObject) if obj.keys.contains("def") =>
        val name = (obj \ "name").validate[JsString].getOrElse(Environment.errorRaising()).value
        val defObj = (obj \ "def").validate[JsObject].getOrElse(Environment.errorRaising())
        (CoqAst.fromJsObject(
          (defObj \ "term").validate[JsObject].getOrElse(Environment.errorRaising()),
          // Need to initialize types for term
          (defObj \ "types").validate[JsArray].getOrElse(Environment.errorRaising())
            .value.map(js => {
            val obj = js.validate[JsObject].get
            val index = (obj \ "index").validate[JsNumber].get.value.toInt
            // Need to translate type key to term key so CoqAst.fromJsObject will work as expected
            val tramsformed = Environment.transformKeys(
              Environment.transformKeys((obj \ "type").validate[JsObject].get, "type", "term"),
              "index",
              "type_index"
            )
            (index,
              CoqAst.fromJsObject(tramsformed.validate[JsObject].get, Map.empty))
          }).toMap),
          name)
      case _ => Environment.errorRaising()
    } map // Turn the asts and names into Definition objects
      {case (ast, name) => Definition(Identifier(name), ast)})
}

object Environment {
  import play.api.libs.json._

  private def errorRaising[T](): T =  throw new IllegalArgumentException("Argument structure not correct. Expecting a __ / def /term and __/ def / types")

  def readJson(input: String): Environment = {
    // TODO: Also return list of types for each var\function
    val json = Json.parse(input) match {
      case x: JsArray => x
      case _ => throw new IllegalArgumentException("Argument should be a list of terms. Did not receive json list.")
    }
    new Environment(json)
  }

  def transformKeys(js: JsValue, orig: String, newKey: String): JsValue = {
    import play.api.libs.json._
    js match {
      case JsObject(fields) =>
        val res = fields.map({
          case (key, value) =>
            (if (key == orig) newKey else orig, transformKeys(value, orig, newKey))
        })
        JsObject(res)
      case JsArray(arr) =>
        JsArray(arr.map(transformKeys(_, orig, newKey)))
      case _ => js
    }
  }
}
