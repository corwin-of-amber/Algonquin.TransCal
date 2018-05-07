import java.io.{File, InputStream}

import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.{Millis, Span}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import relentless.matching._
import relentless.rewriting.HyperEdge
import syntax.Tree


class PatternSpec extends FlatSpec with Matchers with TimeLimitedTests with BeforeAndAfter {

  var trie: Trie[Int, HyperEdge[Int]] = _
  // From each index we can access the higher indexes
  var dir = {
    var tree = new Tree[Trie.DirectoryEntry](-1, 0 to 5 map (new Tree[Trie.DirectoryEntry](_)) toList)
    for (i <- 5 to 0 by -1) {
      tree = tree.replaceDescendant(
        (tree.subtrees(i),
        new Tree[Trie.DirectoryEntry](i, i+1 to 5 map (tree.subtrees(_)) toList))
      )
    }
    tree
  }

  before {
    trie = new Trie[Int, HyperEdge[Int]](dir)
    trie.addAll(Seq(
      HyperEdge(Seq(1, 2, 3, 4)),
      HyperEdge(Seq(1, 2, 3, 4)),
      HyperEdge(Seq(1, 2, 3, 4)),
      HyperEdge(Seq(1, 2, 5, 4)),
      HyperEdge(Seq(3, 4, 2)),
      HyperEdge(Seq(1, 2, 3, 4, 5)),
      HyperEdge(Seq(6, 7, 8, 9))
    ))
  }


  def readFile(fileName: String) = {
    val stream: InputStream = getClass.getResourceAsStream(s"$fileName")
    val test = scala.io.Source.fromInputStream(stream).getLines.toList
    test.length should be > 0
  }

  def readFiles = {
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

  "Lookup" should "return all matching words with all constants" in {
    val pattern = new Pattern(IndexedSeq(1, 2, 3, 4) map HyperTerm)
    val res = pattern.lookup(trie, new Valuation(0))
    res.length shouldBe 4
    all (res) should contain inOrder (1, 2, 3, 4)
  }

  it should "return none if constants dont match" in {
    val pattern = new Pattern(IndexedSeq(1, 2, 9999, 4) map HyperTerm)
    val res = pattern.lookup(trie, new Valuation(0))
    res.length shouldBe 0
  }

  it should "consider already assigned vals" in {
    val pattern = new Pattern(IndexedSeq(HyperTerm(1), HyperTerm(2), Placeholder(0), HyperTerm(4)))
    val valuation = new Valuation(1).unify(new HyperEdge[Int](5, 1, Seq()), new Pattern(IndexedSeq(Placeholder(0), HyperTerm(1)))).get
    val res = pattern.lookup(trie, valuation)
    res.length shouldBe 1
    all (res) should contain inOrder (1, 2, 5, 4)
  }

  it should "with empty trie should be empty" in {
    trie = new Trie[Int, HyperEdge[Int]](dir)
    val pattern = new Pattern(IndexedSeq(1, 2, 3, 4) map HyperTerm)
    val res = pattern.lookup(trie, new Valuation(0))
    res.length shouldBe 0
  }

  it should "manage weirdly ordered valuation" in {
    val pattern = new Pattern(IndexedSeq(Placeholder(1), HyperTerm(2), HyperTerm(3), HyperTerm(4), Placeholder(0)))
    val valuation = new Valuation(2).unify(new HyperEdge[Int](5, 1, Seq()), new Pattern(IndexedSeq(Placeholder(0), Placeholder(1)))).get
    val res = pattern.lookup(trie, valuation)
    res.length shouldBe 1
    all (res) should contain inOrder (1, 2, 3, 4, 5)
  }
}