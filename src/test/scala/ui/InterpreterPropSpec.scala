package ui

import java.io.{ByteArrayOutputStream, PrintStream, File => JFile}

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Minutes, Span}
import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.PatternRewriteRule
import synthesis.search.rewrites.Template.ExplicitTerm
import synthesis.{HyperTermIdentifier, Programs}
import transcallang.{AnnotatedTree, TranscalParser}

import scala.language.higherKinds

class InterpreterPropSpec extends FunSuite with Matchers with TimeLimitedTests with ParallelTestExecution {

  private val parser = new TranscalParser()

  private def abstractTest(fileName: String): ActionSearchState = {
    val userInput: Iterator[AnnotatedTree] = Main.readFile(new JFile(fileName))
    val userOutput: ByteArrayOutputStream = new ByteArrayOutputStream()
    val interpreter = new Interpreter(userInput, new PrintStream(userOutput))
    interpreter.start()
  }

  test("NoDup") {
    val fileName = "src/main/resources/examples/NoDup.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("a1 -> (_ ∉ {x}) /\\ nodup' _ _").subtrees(1))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("NoDup with TimeComplex") {
    val fileName = "src/main/resources/examples/NoDupWithTimeComplex.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("a1 -> timecomplex ({x'} ∪ elems(xs')) (1 + 1 + 1 + 1 + len(xs') + len(xs'))").subtrees(1))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("FilterMap") {
    val fileName = "src/main/resources/examples/FilterMap.tc"
    val lastState = abstractTest(fileName)
    lastState.rewriteRules.flatMap({case x: PatternRewriteRule => Some(x); case _ => None})
      .exists(
        _.premise.edgeTypes.flatMap({case x: ExplicitTerm[HyperTermIdentifier] => Some(x) ; case _ => None})
          .exists(_.value.identifier.literal == "f2")
      )
  }

  test("ConcatMap") {
    val fileName = "src/main/resources/examples/ConcatMap.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.apply("_ ++ _ -> concatMap' _ _ _").subtrees(1))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("Plus Len two datatypes") {
    val fileName = "src/main/resources/examples/thesyPlusLen.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression("plus (len ?x) (len ?y) ||| len (concat x y)"))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

//  test("PrefixSum") {
//    val fileName = "src/main/resources/examples/PrefixSum.tc"
//    val lastState = abstractTest(fileName)
//    val pattern = Programs.destructPattern(parser.apply("a1 -> timecomplex (prefixSum _) _").subtrees(1))
//    val result = lastState.programs.hyperGraph.findSubgraph[Int](pattern)
//    result should not be empty
//  }

  test("thesyNatPlus") {
    val fileName = "src/main/resources/examples/thesyNatSuc.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression(" plus x y ||| plus y x"))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("ReverseReverse") {
    val fileName = "src/main/resources/examples/ReverseReverse.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression("reverse(reverse(x :: y :: ⟨⟩)) ||| x :: y :: ⟨⟩").cleanTypes)
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("thesyFilterFilter") {
    val fileName = "src/main/resources/examples/thesyFilterFilter.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression("filter pred (filter pred (l)) ||| filter pred l"))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  test("thesyReverseReverse") {
    val fileName = "src/main/resources/examples/thesyReverseReverse.tc"
    val lastState = abstractTest(fileName)
    val pattern = Programs.destructPattern(parser.parseExpression("reverse(reverse(f(t))) ||| f(t)"))
    val result = lastState.programs.queryGraph.findSubgraph[Int](pattern)
    result should not be empty
  }

  override def timeLimit: Span = Span(5, Minutes)
}
