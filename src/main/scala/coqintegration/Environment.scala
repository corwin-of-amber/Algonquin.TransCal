package coqintegration

import jdk.jshell.spi.ExecutionControl.NotImplementedException
import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString, JsValue}
import transcallang.Identifier

case class Environment(valueDefinitions: Seq[Definition], typeDefinitions: Seq[DataType]) {
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
    Environment.fromJson(json)
  }

  def fromJson(json: JsArray) = {
    // For each definition save its name and create a coq ast instance
    val defAndTypes = json.value.flatMap {
      case (arr: JsArray) =>
        val obj = arr \ 0
        val kind = (obj \ "kind").validate[String].getOrElse(Environment.errorRaising())

        def process(obj: JsValue) = {
          val name = (obj \ "name").validate[String].getOrElse(Environment.errorRaising())
          val defObj = (obj \ "def").validate[JsArray].getOrElse(Environment.errorRaising())
          Definition(Identifier(name), CoqAst.fromJson(defObj, Seq.empty))
        }

        Environment.JsonKinds.withName(kind) match {
          case Environment.JsonKinds.ConstructRef => throw new NotImplementedException("Shouldn't get this outside of indref")
          // When defining a type we null the def and add constructors in the array
          case Environment.JsonKinds.IndRef if arr.value.length > 1 =>
            val constructors = arr.value.map(process)
            val name = (obj \ "name").validate[String].getOrElse(Environment.errorRaising())
            Right(DataType(Identifier(name), constructors)) +: constructors.map(Left(_))
          case _ => Seq(Left(process(obj.get)))
        }
      case _ => Environment.errorRaising()
    }

    val defs = defAndTypes.collect({case Left(d) => d})
    val types = defAndTypes.collect({case Right(t) => t})
    new Environment(defs, types)
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
