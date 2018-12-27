//package relentless.rewriting
//
//import org.scalatest.concurrent.{Signaler, TimeLimitedTests}
//import org.scalatest.time.{Millis, Span}
//import org.scalatest.{FlatSpec, Matchers}
//import syntax.AstSugar.Term
//
//
//class ReconstructerSpec extends FlatSpec with Matchers with TimeLimitedTests {
//  // TODO use fixtures
//
////  "Entries" should "work well in hashset" in {
////    val entry: Reconstruct.Entry[Int] = Entry(new Tree[Int](1, List.empty), List.empty)
////    val set: mutable.HashSet[Reconstruct.Entry[Int]] = new mutable.HashSet[Entry[Int]]();
////    set shouldNot contain(entry)
////    set.add(entry)
////    set should contain(entry)
////    val entry2: Reconstruct.Entry[Int] = Entry(new Tree[Int](1, List.empty), List.empty)
////    set should contain(entry2)
////    val entry3: Reconstruct.Entry[Int] = Entry(new Tree[Int](2, List.empty), List.empty)
////    set shouldNot contain(entry3)
////  }
//
//  val timeLimit: Span = Span(20000000, Millis)
//  override val defaultTestSignaler: Signaler =
//    new Signaler {
//    override def apply(testThread: Thread): Unit = {
//      println("Kindly die")
//      // using stop because thread will be stuck in long calculation and interrupt wont work
//      testThread.stop() // deprecated. unsafe. do not use
//    }
//  }
//
//  def assertEqualContents(test: ReconstructData): Unit = {
//    val outs = toNewOuts(test)
//    for (tree: Term <- test.expectedOuts) {
//      outs should contain(tree)
//    }
//
//    for (tree: Term <- outs) {
//      test.expectedOuts should contain(tree)
//    }
//  }
//
//  def toNewOuts(test: ReconstructData): List[Term] = {
//    val reconstruct = new Reconstructer(test.init, test.words)
//    reconstruct(test.encoding, test.except).toList
//  }
//
//  "Streaming reconstruct" should "return the full set of possibilities" in {
//    val test = ReconstructData.full
//    assertEqualContents(test)
//  }
//
//  it should "ignore edges in except" in {
//    val test = ReconstructData.except
//    assertEqualContents(test)
//  }
//
//  it should "be able to distinguish branches in circle checks" in {
//    val test = ReconstructData.branches
//    assertEqualContents(test)
//  }
//
//  it should "find a simple term" in {
//    val test = ReconstructData.small
//    assertEqualContents(test)
//  }
//
//  it should "not return doubles" in {
//    val tests = ReconstructData.tests
//    for (test <- tests) {
//      println(s"Starting test")
//      val newOuts: Stream[Term] = {
//        val reconstruct = new Reconstructer(test.init, test.words)
//        reconstruct(test.encoding, test.except)
//      }
//      val treeSet = newOuts toSet;
//      withClue(s"Outputs were ${newOuts}") {newOuts.size shouldEqual treeSet.size}
//    }
//  }
//
//  it should "manage to create trees even if some of the first nodes don't need development" in {
//    val test = ReconstructData.constsOnStart
//    assertEqualContents(test)
//  }
//
//  it should "return entries where the final target is updated late" in {
//    val test = ReconstructData.lateFinal
//    assertEqualContents(test)
//  }
//
//  it should "manage to replace same leaf twice" in {
//    val test = ReconstructData.twoRepeatingLeaves
//    // Ugly way for lazy people to make init an edge and not an int. Important for recreating the bug
//    val reconstruct = new Reconstructer(test.words.head, test.words.drop(1))
//    val outs = reconstruct(test.encoding, test.except).toList
//    for (tree: Term <- test.expectedOuts) {
//      outs should contain(tree)
//    }
//
//    for (tree: Term <- outs) {
//      test.expectedOuts should contain(tree)
//    }
//  }
//}
