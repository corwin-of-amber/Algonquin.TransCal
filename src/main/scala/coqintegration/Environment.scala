package coqintegration

import jdk.jshell.spi.ExecutionControl.NotImplementedException
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString, JsValue}
import transcallang.Identifier

case class Environment(definitions: Seq[Definition]) {

  def this(json: JsArray) = this(
    // For each definition save its name and create a coq ast instance
    json.value.flatMap {
      case (arr: JsArray) =>
        val obj = arr \ 0
        val kind = (obj \ "kind").validate[String].getOrElse(Environment.errorRaising())
        def process(obj: JsValue) = {
          val name = (obj \ "name").validate[String].getOrElse(Environment.errorRaising())
          val defObj = (obj \ "def").validate[JsArray].getOrElse(Environment.errorRaising())
          (CoqAst.fromJson(defObj, Seq.empty), name)
        }
        Environment.JsonKinds.withName(kind) match {
          case Environment.JsonKinds.ConstructRef => throw new NotImplementedException("Shouldn't get this outside of indref")
          // When defining a type we null the def and add constructors in the array
          case Environment.JsonKinds.IndRef if arr.value.length > 1 => arr.value.map(process)
          case _ => Seq(process(obj.get ))
        }
      case _ => Environment.errorRaising()
    } map // Turn the asts and names into Definition objects
      {case (ast, name) => Definition(Identifier(name), ast)})
}

object Environment {
  import play.api.libs.json._

  object JsonKinds extends Enumeration {
    type JsonKinds = Value
    val ConstRef, IndRef, VarRef, ConstructRef = Value
  }

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
