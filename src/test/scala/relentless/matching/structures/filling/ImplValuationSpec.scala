package relentless.matching.structures.filling

import java.io.{File, InputStream}

import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.time.{Millis, Span}
import org.scalatest.{FlatSpec, Matchers}
import relentless.rewriting.HyperEdge


class ImplValuationSpec extends FlatSpec with Matchers with TimeLimitedTests {

  private def readFile(fileName: String) = {
    val stream: InputStream = getClass.getResourceAsStream(s"$fileName")
    val test = scala.io.Source.fromInputStream(stream).getLines.toList
    test.length should be > 0
  }

  private def readFiles = {
    val resources = new File(getClass getResource "." getPath)
    val fileNames = resources.listFiles.filter(_.isFile).filter(_.getName().endsWith(".txt")).toList
    // TODO: remove temporay measure, ignore simple tests as they are in old format
    val tests = for (fn <- fileNames if !(fn.getName contains "simple")) yield readFile(s"/${fn.getName}")
    val streams: Seq[InputStream] = for (fn <- tests) yield getClass.getResourceAsStream(s"/$fn")
    tests.length should be > 0
    tests
  }

  val timeLimit: Span = Span(2000000, Millis)
  implicit val signaler: Signaler = ThreadSignaler

  "Unify" should "take all word to valuation" in {
    val emptyVal = new ImplValuation(5)
    val pattern = new ImplPattern(IndexedSeq(0, 1, 2, 3, 4) map Placeholder)
    val word = HyperEdge(Seq(5, 6, 7, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe true
    newValuation.get should contain theSameElementsInOrderAs (word map(x=>Some(HyperTerm(x))))
  }

  it should "reorder elements as necassery" in {
    val emptyVal = new ImplValuation(5)
    val pattern = new ImplPattern(IndexedSeq(1, 2, 4, 3, 0) map Placeholder)
    val word = HyperEdge(Seq(5, 6, 7, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe true
    newValuation.get should contain inOrderOnly (Some(HyperTerm(9)), Some(HyperTerm(5)), Some(HyperTerm(6)), Some(HyperTerm(8)), Some(HyperTerm(7)))
  }

  it should "keep constants" in {
    val emptyVal = new ImplValuation(2)
    val pattern = new ImplPattern(IndexedSeq(HyperTerm(5), Placeholder(0), Placeholder(1), HyperTerm(8), HyperTerm(9)))
    val word = HyperEdge(Seq(5, 6, 7, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe true
    newValuation.get should contain inOrderOnly (Some(HyperTerm(6)), Some(HyperTerm(7)))
  }

  it should "return None if constants dont match" in {
    val emptyVal = new ImplValuation(2)
    val pattern = new ImplPattern(IndexedSeq(HyperTerm(5), Placeholder(0), Placeholder(1), HyperTerm(8), HyperTerm(1)))
    val word = HyperEdge(Seq(5, 6, 7, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe false
    emptyVal.isDefined(0) shouldBe false
    emptyVal.isDefined(1) shouldBe false
  }

  it should "fail if too many values to fill" in {
    val emptyVal = new ImplValuation(5)
    val pattern = new ImplPattern(IndexedSeq(0, 1, 2, 3, 4, 5) map Placeholder)
    val word = HyperEdge(Seq(5, 6, 7, 8, 9, 10))

    an [Exception] should be thrownBy emptyVal.unify(word, pattern)
  }

  it should "fail we have holes with same valuation index but different values" in {
    val emptyVal = new ImplValuation(1)
    val pattern = new ImplPattern(IndexedSeq(HyperTerm(5), Placeholder(0), Placeholder(0), HyperTerm(8), HyperTerm(9)))
    val word = HyperEdge(Seq(5, 6, 7, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe false
    emptyVal.isDefined(0) shouldBe false
  }

  it should "fill holes with same valuation index but with same values" in {
    val emptyVal = new ImplValuation(1)
    val pattern = new ImplPattern(IndexedSeq(HyperTerm(5), Placeholder(0), Placeholder(0), HyperTerm(8), HyperTerm(9)))
    val word = HyperEdge(Seq(5, 6, 6, 8, 9))

    val newValuation = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe true
    newValuation.get(0) shouldBe Some(HyperTerm(6))
  }

  it should "not change original valuation" in {
    val emptyVal = new ImplValuation(1)
    val pattern = new ImplPattern(IndexedSeq(HyperTerm(5), Placeholder(0), Placeholder(0), HyperTerm(8), HyperTerm(9)))
    val word = HyperEdge(Seq(5, 6, 6, 8, 9))

    val newValuation: Option[Valuation] = emptyVal.unify(word, pattern)

    newValuation.isDefined shouldBe true
    newValuation.get(0) shouldBe Some(HyperTerm(6))
    emptyVal.isDefined(0) shouldBe false
  }
}
