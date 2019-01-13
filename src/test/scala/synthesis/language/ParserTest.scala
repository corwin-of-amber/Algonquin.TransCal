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
    }
  }

  test("Apply alot of abcd") {
    val parsed = p("c a b d")
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 3
    parsed.root shouldEqual "@"
    parsed.subtrees(0).root shouldEqual "@"
    parsed.subtrees(1).root shouldEqual "d"
    parsed.subtrees(0).subtrees(0).root shouldEqual "@"
    parsed.subtrees(0).subtrees(1).root shouldEqual "b"
    parsed.subtrees(0).subtrees(0).subtrees(0).root shouldEqual "c"
    parsed.subtrees(0).subtrees(0).subtrees(1).root shouldEqual "a"
  }

  test("Able to catch parentheses") {
    val parsed = p("c (a b) d")
    parsed.nodes.map(_.root.literal).count(_ == "@") shouldEqual 3
    parsed.root shouldEqual "@"
    parsed.subtrees(0).root shouldEqual "@"
    parsed.subtrees(1).root shouldEqual "d"
    parsed.subtrees(0).subtrees(0).root shouldEqual "c"
    parsed.subtrees(0).subtrees(1).root shouldEqual "@"
    parsed.subtrees(0).subtrees(1).subtrees(0).root shouldEqual "a"
    parsed.subtrees(0).subtrees(1).subtrees(1).root shouldEqual "b"
  }

  test("Can have annotations") {
    val parsed = p("_ [Anno]")
    parsed.root shouldEqual "Annotation"
    parsed.root.kind shouldEqual "Anno"
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
    val parsed = p("a = b /\\ c < b -> d ∈ e")
    parsed.root shouldEqual "->"
    parsed.subtrees(0).root shouldEqual "∧"
    parsed.subtrees(0).subtrees(0).root shouldEqual "="
    parsed.subtrees(0).subtrees(0).subtrees(0).root shouldEqual "a"
    parsed.subtrees(0).subtrees(0).subtrees(1).root shouldEqual "b"
    parsed.subtrees(0).subtrees(1).root shouldEqual "@"
    parsed.subtrees(0).subtrees(1).subtrees(0).root shouldEqual "@"
    parsed.subtrees(0).subtrees(1).subtrees(1).root shouldEqual "b"
    parsed.subtrees(0).subtrees(1).subtrees(0).subtrees(0).root shouldEqual "<"
    parsed.subtrees(0).subtrees(1).subtrees(0).subtrees(1).root shouldEqual "c"
    parsed.subtrees(1).root shouldEqual "@"
    parsed.subtrees(1).subtrees(0).subtrees(0).root shouldEqual "∈"
    parsed.subtrees(1).subtrees(0).subtrees(1).root shouldEqual "d"
    parsed.subtrees(1).subtrees(1).root shouldEqual "e"
  }
}

class OldParserTest extends ParserTest(new OldParser())

class TranscalParserTest extends ParserTest(new TranscalParser) {
  protected val parser = p.asInstanceOf[TranscalParser]
}