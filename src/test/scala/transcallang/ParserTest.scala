package transcallang

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
    val parsed = p("f = _ ;g = _")
    parsed.root shouldEqual ";"
    parsed.subtrees(0).subtrees(0).root shouldEqual "f"
    parsed.subtrees(1).subtrees(0).root shouldEqual "g"
  }

  test("Can have two statements with newline") {
    val parsed = p("f = _ \ng = _")
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
    val t1 = p("min(x, y) -> (x ≤ y)")
    val t2 = p("_ -> id(x)")
    val parsed = p("(x ≤ y) ||> min(x, y) >> id(x)")
    parsed.root shouldEqual ">>"
    parsed.subtrees(0).root.literal shouldEqual "||>"
    parsed.subtrees(0).subtrees(0) shouldEqual t1.subtrees(1)
    parsed.subtrees(1) shouldEqual t2.subtrees(1)
  }

  test("split lambdas with params works correctly") {
    val anno = (new TranscalParser).apply("concat ?l = l match ((⟨⟩ => ⟨⟩) / (?xs :: ?xss) => xs ++ concat xss)   [++]")
    anno.root shouldEqual Language.annotationId
    anno.subtrees(1).root.literal shouldEqual "++"
    val parsed = anno.subtrees(0)
    parsed.root shouldEqual Language.letId
    parsed.subtrees(0).root.literal shouldEqual "concat"
    parsed.subtrees(1).root shouldEqual Language.matchId
    val lhs = parsed.subtrees(1).subtrees(0)
    val rhs1 = parsed.subtrees(1).subtrees(1)
    val rhs2 = parsed.subtrees(1).subtrees(2)
    lhs.root.literal shouldEqual "l"
    rhs1.root shouldEqual Language.guardedId
    rhs1.subtrees(0).root shouldEqual Language.nilId
    rhs1.subtrees(1).root shouldEqual Language.nilId

    rhs2.root shouldEqual Language.guardedId
    rhs2.subtrees(0).root shouldEqual Language.consId
    rhs2.subtrees(0).subtrees(0).root.literal shouldEqual "?xs"
    rhs2.subtrees(0).subtrees(1).root.literal shouldEqual "?xss"
    rhs2.subtrees(1).root.literal shouldEqual "++"
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

  test("Parse match statement") {
    val parsed = (new TranscalParser).apply("?x -> x match (1 => 1) / (_ => 0)").subtrees(1)
    parsed.root shouldEqual Language.matchId
    parsed.subtrees(0).root.literal shouldEqual "x"
    parsed.subtrees(1).root shouldEqual Language.guardedId
    parsed.subtrees(2).root shouldEqual Language.guardedId
    parsed.subtrees(2).subtrees(0).root shouldEqual Language.holeId
    parsed.subtrees(2).subtrees(1).root.literal shouldEqual 0
  }

  test("Parse simple rewrite rules") {
    val rules = Seq("~true = false",
    "~false = true",
    "id (?x) >> x",

    "?x == ?x' = x' == x",
    "?x == ?x' = x' ∈ { x }",
    "elem(?x, (?x' :: ?xs')) = ((x == x') \\/ elem(x, xs'))",
    "~(?x == ?y) = (x != y)",
    "~(?x ∈ ?y) = (x ∉ y)",
    "(?x ∉ ?xs) = { x } ‖ xs",
    "~(?x \\/ ?y) = (~x /\\ ~y)",
    "~(?x /\\ ?y) = (~x \\/ ~y)",
    "((?x ‖ ?xs) /\\ (?y ‖ xs)) = ((x ∪ y) ‖ xs)",
    "((?xs ‖ ?x) /\\ (xs ‖ ?y)) = (xs ‖ (x ∪ y))",
    "?x ∈ (?xs ∪ ?ys) = (x ∈ xs) \\/ (x ∈ ys)",
    "elem(?x, ?xs) = x ∈ elems(xs)",
    "elems(?x' :: ?xs') = ({x'} ∪ elems(xs'))", // <-- this one is somewhat superfluous?

    "(?y :+ ?x) = (y ++ (x :: ⟨⟩))",
    "⟨⟩ ++ ?xs' >> id xs'",
    "?xs' ++ ⟨⟩ >> id xs'",

    "((?x < ?y) ||| true) >> (x ≤ y)",
    "(?x ≤ ?y) ||> min(x, y) >> id x",
    "(?x ≤ ?y) ||> min(y, x) >> id x",
    //    min(x, y) =:> min(y,x),

    "(?x ≤ ?y) ||> bounded_minus(x, y) >> 0",

    "(take ?xs 0) >> ⟨⟩",
    "(take ?xs (len xs)) >> id xs",
    "(take (?xs ++ ?xs') ?x) >> ((take xs (min len(xs) x)) ++ (take xs' (bounded_minus x len xs)))",

    // merge range
    "(range_exclude(?x, ?y) ++ range_exclude(y, ?z)) >> range_exclude(x, z)",
    // exclude to include
    "range_exclude(?x, ?y + 1) = range_include(x, y)",
    // singleton range
    "range_include(?x, x) = (x :: ⟨⟩)",
    "(?z ∈ range_exclude(?x, ?y) ||| true) >> ((x ≤ z) ||| (z < y))")

    val parser = new TranscalParser
    rules.map(parser(_))
  }
}

//class OldParserTest extends ParserTest(new OldParser())

class TranscalParserTest extends ParserTest(new TranscalParser) {
  protected val parser = p.asInstanceOf[TranscalParser]
}