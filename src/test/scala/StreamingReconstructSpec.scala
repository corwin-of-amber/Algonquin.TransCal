import java.io.{File, InputStream}

import org.json4s.DefaultReaders.{JArrayReader, JObjectReader}
import org.json4s.JsonAST.{JArray, JObject}
import org.scalatest.{FlatSpec, Matchers}
import syntax.Tree
import org.json4s.native.JsonMethods._
import org.json4s._
import org.scalatest.concurrent.{Interruptor, TimeLimitedTests}
import org.scalatest.time.{Millis, Seconds, Span}
import relentless.rewriting.Reconstruct
import relentless.rewriting.Reconstruct.Entry
import syntax.AstSugar.Term

import scala.collection.mutable


class StreamingReconstructSpec extends FlatSpec with Matchers with TimeLimitedTests {
  def jarrayToTree(arr: Iterable[Any]): Tree[Int] = {
    val root: Int = arr.head.toString.toInt
    val subtrees = for (obj <- arr.tail) yield {
      obj match {
        case JInt(x) => new Tree[Int](x.toInt)
        case l: JArray => jarrayToTree(l.values)
        case a: List[JValue] => jarrayToTree(a)
        case num: BigInt => new Tree[Int](num.toInt)
        case x => throw new RuntimeException(x.getClass toString)
      }
    }
    new Tree[Int](root, subtrees toList)
  }

  def treeArrayToTrees(arr: JArray): Seq[Tree[Int]] = {
    for (obj <- arr.values) yield {
      obj match {
        case l: List[JValue] => jarrayToTree(l)
        case a: JArray => jarrayToTree(a.values)
        case x => throw new RuntimeException(x.getClass toString)
      }
    }
  }

  def readFile(fileName: String) = {
    val stream: InputStream = getClass.getResourceAsStream(s"$fileName")
    val test = scala.io.Source.fromInputStream(stream).getLines.toList
    test.length should be > 0
    new {
      val name: String = fileName
      val finals: Array[Int] = test(1) split " " map (_.toInt)
      val init: Tree[Int] = jarrayToTree(parse(test(0)).as[JArray].values)
      val except: Set[Int] = test(2) split " " filter (_.length > 0) map (_ toInt) toSet
      val outs: List[Tree[Int]] = treeArrayToTrees(parse(test(3)).as[JArray]) toList
      val words: Seq[Array[Int]] = test drop 4 map ((line: String) => line split " " map (_.toInt))
      val newOuts: List[Tree[Int]] = (new Reconstruct(init.root, words) ++ (finals zip Stream.continually(null)).toMap) (except) toList
    }
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

  // TODO use fixtures

  "Entries" should "work well in hashset" in {
    val entry: Reconstruct.Entry[Int] = Entry(new Tree[Int](1, List.empty), List.empty)
    val set: mutable.HashSet[Reconstruct.Entry[Int]] = new mutable.HashSet[Entry[Int]]();
    set shouldNot contain(entry)
    set.add(entry)
    set should contain(entry)
    val entry2: Reconstruct.Entry[Int] = Entry(new Tree[Int](1, List.empty), List.empty)
    set should contain(entry2)
    val entry3: Reconstruct.Entry[Int] = Entry(new Tree[Int](2, List.empty), List.empty)
    set shouldNot contain(entry3)
  }

  val timeLimit: Span = Span(20000000, Millis)
  val defaultTestInterruptor: Interruptor =
    new Interruptor {
    override def apply(testThread: Thread): Unit = {
      println("Kindly die")
      // using sto because thread will be stuck in long calculation and interrupt wont work
      testThread.stop() // deprecated. unsafe. do not use
    }
  }

  "Streaming reconstruct" should "return the full set of possibilities" in {
    val test = readFile("/full.txt")
    // Test structure is defined in Test Creator
    for (tree: Tree[Int] <- test.outs) {
      test.newOuts should contain(tree)
    }

    for (tree: Tree[Int] <- test.newOuts) {
      test.outs should contain(tree)
    }
    println("results for full set are:")
    println(test.newOuts mkString " ")
  }

  it should "ignore edges in except" in {
    val test = readFile("/except.txt")
    // Test structure is defined in Test Creator
    for (tree: Tree[Int] <- test.outs) {
      test.newOuts should contain(tree)
    }

    for (tree: Tree[Int] <- test.newOuts) {
      test.outs should contain(tree)
    }

    println("results for except test are:")
    println(test.newOuts mkString " ")
  }

  it should "be able to distinguish branches in circle checks" in {
    val test = readFile("/branches.txt")
    // Test structure is defined in Test Creator
    for (tree: Tree[Int] <- test.outs) {
      test.newOuts should contain(tree)
    }

    for (tree: Tree[Int] <- test.newOuts) {
      test.outs should contain(tree)
    }

    println("results for circle testing are:")
    println(test.newOuts mkString " ")
  }

  it should "not return doubles" in {
    val tests = readFiles
    for (test <- tests.drop(1)) {
      println(s"Starting test: ${test.name}")
      val treeSet = test.newOuts toSet;
      withClue(s"Outputs were ${test.newOuts}") {test.newOuts.size shouldEqual treeSet.size}
    }
  }

  it should "manage to create trees even if some of the first nodes don't need development" in {
    /*
    So I am taking the case from the problem with locate:
    44 -> 1 identifier
    43 -> 2 identifier
    38 -> 3 known term
    39 -> 4 known term
    42 -> 5
    7 -> 6 identifier
    40 -> 7
    37 -> 8
    init = 44{42 38 39} -> 1{5 3 4}
    Finals = 44{43, 38, 39} -> 1{2 3 4}
    know terms = Set(56, 37, 25, 57, 28, 38, 2, 34, 54, 39, 35, 31, 40, 55, 58, 36, 30)
    Rewrites:
1 2
10 2 34 35
27 34 25 28
24 25
29 28
27 35 36 37
15 36 30 31
32 30
33 31
20 37 38 39
7 38 40
14 40 30 31
1 39 31
1 28 25
43 42 -> 2 5
44 37 42 38 39 -> 1 8 5 3 4
41 37 42
22 51 31
12 40 30 51
1 37 36
18 38 30 51
13 52 30
19 38 52 51
     */
    val words = Seq(Seq(2, 5).toArray, Seq(1, 8, 5, 3, 4).toArray).toStream
    val im: Map[Int, Term] = Map(3 -> null, 4 -> null)
    val expected = Reconstruct.tupleToTree(Seq(1, -1, 2, 3, 4).toArray)
    val reconstruct = new Reconstruct(Seq(1, -1, 5, 3, 4).toArray, words) ++ im
    val resultTree: Tree[Int] = reconstruct(Set[Int]()).head
    resultTree should equal (expected)
  }

  it should "return entries where the final target is updated late" in {
    /*
    So I am taking the case from the another problem with locate:
    init = 44{82} (44 81 82) -> 1{4}
    Finals = 44{83} -> 1{2}
    know terms = Set(69, 138, 56, 37, 25, 57, 61, 132, 133, 74, 60, 28, 38, 137, 65, 129, 134, 73, 128, 2, 34, 64, 71, 54, 39, 66, 130, 135, 35, 63, 31, 72, 40, 55, 139, 75, 58, 36, 30, 136, 79, 131, 68, 62)
    identifiers = 14 21 22 80 7 27 59 1 70 29 17 44 67 18 32 76 19 15 10 78 77 83 41 16 33 12 13 11 20 24
    */
    val rewrites: String = """1 2
10 2 34 35
27 34 25 28
24 25
29 28
27 35 36 37
15 36 30 31
32 30
33 31
20 37 38 39
7 38 40
14 40 30 31
1 39 31
41 37 42
19 38 61 51
13 61 30
22 51 31
59 37 61 31
27 64 65 66
67 65
27 66 68 69
70 68
20 69 71 72
19 71 65 73
22 73 68
1 72 68
59 81 79 31
80 79
1 28 25
77 75
76 74
15 31 74 75
12 40 30 51
1 37 36
18 38 30 51
59 69 65 68
19 84 79 51
20 81 84 39
78 81
14 85 30 75
11 86 30 74
16 40 86 85
22 87 75
13 88 74
21 51 88 87
1 89 75
14 90 74 75
7 91 90
20 39 91 89
83 82
44 81 82
41 81 82
12 85 30 87
12 86 74 61
7 93 85
7 94 86
20 38 94 93
19 93 61 87
19 94 61 88
19 97 79 87
19 98 79 88
20 84 98 97
12 90 74 87
20 99 38 91
20 37 99 89
20 101 84 91
20 81 101 89
18 93 30 87
17 94 30 74
18 94 74 61
20 103 93 39
20 37 94 103
18 94 30 88
18 98 74 79
20 106 97 39
20 81 98 106
18 91 74 87
16 107 40 90
7 99 107
20 108 93 91
20 99 94 108
20 111 97 91
20 101 98 111
19 94 88 61
20 103 108 89
12 113 30 88
7 94 113
12 114 74 79
7 98 114
19 98 88 79
20 106 111 89
19 91 88 87
16 115 85 90
7 108 115
11 113 74 30
16 120 113 85
7 38 120
21 121 61 88
19 108 121 87
21 122 79 88
19 111 122 87
59 39 88 75
16 123 86 115
7 99 123
16 124 113 115
7 99 124
17 94 74 30
16 125 120 90
7 99 125
59 103 121 75
59 106 122 75"""
    val words = rewrites.split("\n") map ((s) => s.split(" ") map (_.toInt) toArray)
    val im: Map[Int, Term] = Set(69, 138, 56, 37, 25, 57, 61, 132, 133, 74, 60, 28, 38, 137, 65, 129, 134, 73, 128, 2, 34, 64, 71, 54, 39, 66, 130, 135, 35, 63, 31, 72, 40, 55, 139, 75, 58, 36, 30, 136, 79, 131, 68, 62).zip(Stream.continually(null)).toMap
    val expected = Reconstruct.tupleToTree(Seq(44, -1 , 83).toArray)
    val reconstruct = new Reconstruct(Seq(44, 81, 82).toArray, words.toStream) ++ im
    val resultTree: Tree[Int] = reconstruct(Set[Int]()).head
    resultTree should equal (expected)
  }

}
