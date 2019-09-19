package synthesis.ui

import java.io.{ByteArrayOutputStream, PrintStream, File => JFile}

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Minutes, Span}
import org.scalatest.{FunSuite, Matchers}
import synthesis.actions.ActionSearchState
import synthesis.rewrites.RewriteRule
import synthesis.rewrites.Template.ExplicitTerm
import synthesis.ui.Main.readJFile
import synthesis.{HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, TranscalParser}

import scala.language.higherKinds

class InterpreterPropSpec extends FunSuite with Matchers with TimeLimitedTests {

  private val parser = new TranscalParser()

  private def abstractTest(fileName: String): ActionSearchState = {
    val wd = new JFile(".").getCanonicalPath
    val userInput: Iterator[AnnotatedTree] = readJFile(new JFile(fileName))
    val userOutput: ByteArrayOutputStream = new ByteArrayOutputStream()
    val interpreter = new Interpreter(userInput, new PrintStream(userOutput))
    interpreter.start()
  }

  test("NoDup") {
    val fileName = "src/main/resources/examples/NoDup.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("(1) -> (_ ∉ {x}) /\\ nodup' _ _").subtrees(1))
    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("NoDup with TimeComplex") {
    val fileName = "src/main/resources/examples/NoDupWithTimeComplex.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("(1) -> timecomplex ({x'} ∪ elems(xs')) (1 + 1 + 1 + 1 + len(xs') + len(xs'))").subtrees(1))
    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("FilterMap") {
    val fileName = "src/main/resources/examples/FilterMap.tc"
    val lastState = abstractTest(fileName)
    lastState.rewriteRules.flatMap({case x: RewriteRule => Some(x); case _ => None})
      .exists(
        _.premise.edgeTypes.flatMap({case x: ExplicitTerm[HyperTermIdentifier] => Some(x) ; case _ => None})
          .exists(_.value.identifier.literal == "f2")
      )
  }

  test("ConcatMap") {
    val fileName = "src/main/resources/examples/ConcatMap.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("_ ++ _ -> concatMap' _ _ _").subtrees(1))
    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("PrefixSum") {
    val fileName = "src/main/resources/examples/PrefixSum.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("a1 -> timecomplex (prefixSum _) _").subtrees(1))
    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("ReverseReverse") {
    val fileName = "src/main/resources/examples/ReverseReverse.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression("reverse(reverse(x :: y :: ⟨⟩)) ||| x :: y :: ⟨⟩"))
    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  override def timeLimit: Span = Span(3, Minutes)
}
