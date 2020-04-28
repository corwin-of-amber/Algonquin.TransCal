package coqintegration

import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import org.scalatest.concurrent.TimeLimitedTests
import transcallang.Language

import scala.io.Source

class CoqAstTest  extends FunSuite with Matchers{

  val definitionsFile = "src/main/resources/theories/Definitions.json"
  val newInductiveFile = "src/main/resources/theories/NewInductive.json"

  test("Definitions - find constructor names") {
    val environment = Environment.readJson(Source.fromFile(definitionsFile).mkString)
    environment.definitions.exists(_.name.literal.endsWith("foo")) shouldEqual (true)
    val const_number = environment.definitions.find(_.name.literal.endsWith("const_number"))
    const_number should not be empty
    const_number.get.ast.subtrees.head.root.literal should endWith ("S")
  }

  test("Definitions - flatten function calls") {
    val environment = Environment.readJson(Source.fromFile(definitionsFile).mkString)
    val const_number = environment.definitions.find(_.name.literal.endsWith("const_number"))
    const_number should not be empty
    const_number.get.ast.root shouldNot equal (Language.applyId)
  }

  test("NewInductive - includes constructor definitions with name") {
    val environment = Environment.readJson(Source.fromFile(newInductiveFile).mkString)
    val a = environment.definitions.find(_.name.literal == "A")
    a should not be empty
    a.get.ast.expressionType should not be empty
    val typeA = a.get.ast.expressionType.get
    typeA.root.literal shouldEqual "TheSy"
    typeA.subtrees.head.root.literal shouldEqual "E"
    val thesyE = typeA
    val b = environment.definitions.find(_.name.literal == "B")
    b should not be empty
    b.get.ast.expressionType should not be empty
    val typeB = b.get.ast.expressionType.get
    typeB.root shouldEqual Language.mapTypeId
    typeB.subtrees.head.root.literal shouldEqual "E"
    typeB.subtrees.last shouldEqual Language.mapTypeId
    typeB.subtrees.last.subtrees.foreach({t =>
      t.shouldEqual(thesyE)
    })
  }
}
