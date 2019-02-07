package language

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Inspectors, Matchers}
import syntax.AstSugar.Term

import scala.io.Source

abstract class ParserTest(protected val p: Parser[Term]) extends FunSuite with Matchers with Inspectors with Checkers with LazyLogging {
  test("testApply") {
    val path = getClass.getResource("/").getPath + "../classes/examples"
    for (f <- new File(path).listFiles().filter(_.getName.endsWith(".tc"))) {
      logger.info(s"Working on $f")
      val text = Source.fromFile(f).getLines().mkString("\n")
      logger.info(text)
      logger.info("")
      val term = p.apply(text)
      term shouldEqual p.apply(text)
      logger.info(s"Finished test. Result Term is $term")
    }
  }

  test("Apply alot of abcd") {
    val parsed = p("_ -> c a b d").subtrees(1)
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 0
    parsed.root shouldEqual "c"
    parsed.subtrees(0).root shouldEqual "a"
    parsed.subtrees(1).root shouldEqual "b"
    parsed.subtrees(2).root shouldEqual "d"
  }

  test("Able to catch parentheses") {
    val parsed = p("f = c (a b) d").subtrees(1)
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 0
    parsed.root shouldEqual "c"
    parsed.subtrees(0).root shouldEqual "a"
    parsed.subtrees(1).root shouldEqual "d"
    parsed.subtrees(0).subtrees(0).root shouldEqual "b"
  }

  test("Can have annotations") {
    val parsed = p("f = _ [Anno]")
    parsed.root shouldEqual "Annotation"
    parsed.subtrees(1).root shouldEqual "Anno"
    parsed.subtrees(0).subtrees(1).root shouldEqual "_"
  }

  test("Can have two statements") {
    val parsed = p("f = _ ; g = _")
    parsed.root shouldEqual ";"
    parsed.subtrees(0).subtrees(0).root shouldEqual "f"
    parsed.subtrees(1).subtrees(0).root shouldEqual "g"
  }

  test("Can have two statements with newline") {
    val parsed = p("f = _ \n g = _")
    parsed.root shouldEqual ";"
    parsed.subtrees(0).subtrees(0).root shouldEqual "f"
    parsed.subtrees(1).subtrees(0).root shouldEqual "g"
  }

  test("Parse alot of booleans") {
    val parsed = p("(a == b) /\\ (c < b) -> d ∈ e")
    parsed.root shouldEqual "->"
    // Next function is and and has 2 params so 2 applys
    parsed.subtrees(0).root shouldEqual "∧"
    parsed.subtrees(0).subtrees(0).root shouldEqual "=="

    // First parameter is = which has 2 params so 2 applys
    parsed.subtrees(0).subtrees(0).subtrees(0).root shouldEqual "a"
    parsed.subtrees(0).subtrees(0).subtrees(1).root shouldEqual "b"
    parsed.subtrees(0).subtrees(1).root shouldEqual "<"
    parsed.subtrees(0).subtrees(1).subtrees(0).root shouldEqual "c"
    parsed.subtrees(0).subtrees(1).subtrees(1).root shouldEqual "b"

    // Second part of drags is set_in
    parsed.subtrees(1).root shouldEqual "∈"
    parsed.subtrees(1).subtrees(0).root shouldEqual "d"
    parsed.subtrees(1).subtrees(1).root shouldEqual "e"
  }

  test("Parse tuples and parenthesised expressions") {
    val parsed = p("(a,) 0 -> (a)")
    parsed.root shouldEqual "->"
    parsed.subtrees(0).root shouldEqual Language.tupleId
    parsed.subtrees(0).subtrees(0) shouldEqual parsed.subtrees(1)
  }

  test("Syntax sugar for function definition should create lhs apply") {
    val parsed = p("f = ?x ↦ x + 1")
    parsed.root shouldEqual "="
    parsed.subtrees(0).root.literal shouldEqual "f"
    parsed.subtrees(0).subtrees(0).root.literal shouldEqual "?x"
  }

  test("apply of f translated correctly") {
    val parsed = p("f ?x = x + 1")
    parsed.root shouldEqual "="
    parsed.subtrees(0).root.literal shouldEqual "f"
  }

  test("apply on tuple becomes flattened apply") {
    val parsed = p("f(?x, ?y) = f x y")
    parsed.root shouldEqual "="
    parsed.subtrees(0).root.literal shouldEqual "f"
    parsed.subtrees(0).subtrees(0).root.literal shouldEqual "?x"
    parsed.subtrees(1).subtrees(0).root.literal shouldEqual "x"
  }

  test("use the condition builders") {
    val t1 = p("_ -> (x ≤ y)")
    val t2 = p("min(x, y) -> id(x)")
    val parsed = p("(x ≤ y) ||> min(x, y) -> id(x)")
    parsed.root shouldEqual "||>"
    parsed.subtrees(0).root.literal shouldEqual "≤"
    parsed.subtrees(1).root.literal shouldEqual "->"
  }

  test("split lambdas with params works correctly") {
    val anno = (new TranscalParser).apply("concat = ((⟨⟩ ↦ ⟨⟩) / (?xs :: ?xss) ↦ xs ++ concat xss)   [++]")
    anno.root shouldEqual Language.annotationId
    anno.subtrees(1).root.literal shouldEqual "++"
    val parsed = anno.subtrees(0)
    parsed.root shouldEqual Language.letId
    parsed.subtrees(0).root.literal shouldEqual "concat"
    parsed.subtrees(1).root shouldEqual Language.splitId
    val lhs = parsed.subtrees(1).subtrees(0)
    val rhs = parsed.subtrees(1).subtrees(1)
    lhs.root shouldEqual Language.lambdaId
    lhs.subtrees(0).root shouldEqual Language.nilId
    lhs.subtrees(1).root shouldEqual Language.nilId

    rhs.root shouldEqual Language.lambdaId
    rhs.subtrees(0).root shouldEqual Language.consId
    rhs.subtrees(0).subtrees(0).root.literal shouldEqual "?xs"
    rhs.subtrees(0).subtrees(1).root.literal shouldEqual "?xss"
    rhs.subtrees(1).root.literal shouldEqual "++"
  }

  test("Parser adds clojure when needed") {
    val parsed = (new TranscalParser).apply("_ -> ?x ↦ ?y ↦ x + y")
    parsed.root shouldEqual Language.tacticId
    parsed.subtrees(0).root.literal shouldEqual "_"
    val rhs = parsed.subtrees(1)
    rhs.root shouldEqual Language.lambdaId
    rhs.subtrees(0).root.literal shouldEqual "?x"
    rhs.subtrees(1).root shouldEqual Language.applyId
    rhs.subtrees(1).subtrees(1).root.literal shouldEqual "x"
  }

  test("Adding clojure renames variables") {
    val parsed = (new TranscalParser).apply("_ -> ?x ↦ ?y ↦ x + y")
    val rhs = parsed.subtrees(1)
    rhs.subtrees(1).root shouldEqual Language.applyId
    rhs.subtrees(1).subtrees(0).subtrees(0).subtrees(0).root.literal should not equal "?x"
    rhs.subtrees(1).subtrees(1).root.literal shouldEqual "x"
    rhs.subtrees(1).subtrees(0).subtrees(1).leaves.map(_.root.literal) should not contain "x"
  }

  test("Parser doesnt add unneeded clojures") {
    val parsed = (new TranscalParser).apply("_ -> ?x ↦ ?y ↦ y")
    parsed.root shouldEqual Language.tacticId
    parsed.subtrees(0).root.literal shouldEqual "_"
    parsed.subtrees(1).root shouldEqual Language.lambdaId
    val rhs = parsed.subtrees(1)
    rhs.root shouldEqual Language.lambdaId
    rhs.subtrees(0).root.literal shouldEqual "?x"
    rhs.subtrees(1).root shouldEqual Language.lambdaId
  }
}

//class OldParserTest extends ParserTest(new OldParser())

class TranscalParserTest extends ParserTest(new TranscalParser) {
  protected val parser = p.asInstanceOf[TranscalParser]
}