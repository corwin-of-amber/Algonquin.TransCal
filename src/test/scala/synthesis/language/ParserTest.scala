package synthesis.language

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSuite, Inspectors, Matchers}
import syntax.AstSugar.Term

import scala.io.Source

abstract class ParserTest(protected val p: Parser[Term]) extends FunSuite with Matchers with Inspectors with LazyLogging {
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
    val parsed = p("c a b d")
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 1
    parsed.root shouldEqual "@"
    parsed.subtrees(0).root shouldEqual "c"
    parsed.subtrees(1).root shouldEqual "a"
    parsed.subtrees(2).root shouldEqual "b"
    parsed.subtrees(3).root shouldEqual "d"
  }

  test("Able to catch parentheses") {
    val parsed = p("c (a b) d")
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 2
    parsed.root shouldEqual "@"
    parsed.subtrees(0).root shouldEqual "c"
    parsed.subtrees(1).root shouldEqual "@"
    parsed.subtrees(1).subtrees(0).root shouldEqual "a"
    parsed.subtrees(1).subtrees(1).root shouldEqual "b"
    parsed.subtrees(2).root shouldEqual "d"
  }

  test("Can have annotations") {
    val parsed = p("_ [Anno]")
    parsed.root shouldEqual "Annotation"
    parsed.subtrees(1).root shouldEqual "Anno"
    parsed.subtrees(0).root shouldEqual "_"
  }

  test("Can have two statements") {
    val parsed = p("_;_")
    parsed.root shouldEqual ";"
    parsed.subtrees(0).root shouldEqual "_"
    parsed.subtrees(1).root shouldEqual "_"
  }

  test("Can have two statements with newline") {
    val parsed = p("_ \n _")
    parsed.root shouldEqual ";"
    parsed.subtrees(0).root shouldEqual "_"
    parsed.subtrees(1).root shouldEqual "_"
  }

  test("Parse alot of booleans") {
    val parsed = p("(a = b) /\\ (c < b) -> d ∈ e")
    parsed.root shouldEqual "->"
    // Next function is and and has 2 params so 2 applys
    parsed.subtrees(0).root shouldEqual "@"
    parsed.subtrees(0).subtrees(0).root shouldEqual "∧"

    // First parameter is = which has 2 params so 2 applys
    parsed.subtrees(0).subtrees(1).root shouldEqual "@"
    parsed.subtrees(0).subtrees(1).subtrees(0).root shouldEqual "="
    parsed.subtrees(0).subtrees(1).subtrees(1).root shouldEqual "a"
    parsed.subtrees(0).subtrees(1).subtrees(2).root shouldEqual "b"

    // Second param is < with c and b
    parsed.subtrees(0).subtrees(2).root shouldEqual "@"
    parsed.subtrees(0).subtrees(2).subtrees(0).root shouldEqual "<"
    parsed.subtrees(0).subtrees(2).subtrees(1).root shouldEqual "c"
    parsed.subtrees(0).subtrees(2).subtrees(2).root shouldEqual "b"

    // Second part of drags is set_in
    parsed.subtrees(1).root shouldEqual "@"
    parsed.subtrees(1).subtrees(0).root shouldEqual "∈"
    parsed.subtrees(1).subtrees(1).root shouldEqual "d"
    parsed.subtrees(1).subtrees(2).root shouldEqual "e"
  }
}

class OldParserTest extends ParserTest(new OldParser())

class TranscalParserTest extends ParserTest(new TranscalParser) {
  protected val parser = p.asInstanceOf[TranscalParser]
}