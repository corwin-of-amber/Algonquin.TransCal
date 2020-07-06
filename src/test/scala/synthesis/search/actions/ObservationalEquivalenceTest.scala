package synthesis.search.actions

import org.scalatest.{FunSuite, Matchers, ParallelTestExecution}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.rewrites.{AssociativeRewriteRulesDB, SimpleRewriteRulesDB, SystemRewriteRulesDB}
import transcallang.{AnnotatedTree, Language, TranscalParser}

class ObservationalEquivalenceTest extends FunSuite with ScalaCheckPropertyChecks with Matchers with ParallelTestExecution {
  val parser = new TranscalParser
  val normalRules = SystemRewriteRulesDB.rewriteRules ++ AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules

  test("testFromTerms") {
    val eqiuivalentTerms = Seq(
      "x + y",
      "y + x",
      s"x :: y :: ${Language.nilId.literal}",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal})",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal}) ++ ${Language.nilId.literal}"
    ).map(parser.parseExpression)
    val res = new ObservationalEquivalence(3).fromTerms(eqiuivalentTerms, normalRules)
    val expected = Set(eqiuivalentTerms.take(2).toSet, eqiuivalentTerms.drop(2).toSet)
    expected shouldEqual res
  }

  test("testGetEquives") {
    val terms = "(x + y + z),(z + x + y),(j + k + z),(j + k),(k + j)".split(",").map(parser.parseExpression)
    val programs = Programs.empty.addTerm(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, terms))
    val ids = terms.map(t => {
      val (graph, root) = Programs.destructPatternWithRoot(t)
      programs.queryGraph.findSubgraph[Int](graph).head.nodeMap(root.id)
    }).toSeq
    val res = new ObservationalEquivalence(5).getEquives(new ActionSearchState(programs, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    Set(Set(ids take 2: _*), Set(ids.slice(3, 5): _*), Set(ids(2))) shouldEqual res._2.map(_.filter(ids.contains)).filter(_.nonEmpty)
  }

  private def checkUnionFlatten(numsLen: Int, splitAt: Set[Set[Int]]) = {
    val nums = (0 to numsLen).toSet
    val splitters = splitAt.map(s => (s.map(math.abs(_) % nums.size) + 0 + (nums.size)).toSeq.sorted)
    val equives = splitters.toSeq.map(s => s.sliding(2).toSeq.map(x => {
      nums.slice(x(0), x(1))
    }).toSet)
    val flattened = ObservationalEquivalence.flattenUnionConclusions(equives)
    val allNums = equives.flatten.flatten
    val flattenedMap = flattened.flatMap(s => s.map(x => x -> s)).toMap
    equives foreach (ss => ss foreach (s => s subsets 2 take 10 foreach (combs => {
      flattenedMap(combs.head) should contain (combs.last)
      flattenedMap(combs.last) foreach (i => flattenedMap(i) should contain (combs.head))
    })))
  }

  test("test flatten union doesn't miss sets") {
    forAll (maxDiscardedFactor(500.0), SizeRange(10)) { (numsLen: Byte, splitAt: Set[Set[Int]]) =>
      whenever(numsLen > 0 && numsLen < 30 && splitAt.nonEmpty && splitAt.forall(_.nonEmpty)) {
        checkUnionFlatten(numsLen, splitAt)
      }
    }
  }

  def checkIntersectionFlatten(numsLen: Int, splitAt: Set[Set[Int]]) = {
    val nums = (0 to numsLen).toSet
    val splitters = splitAt.map(s => (s.map(math.abs(_) % nums.size) + 0 + (nums.size - 1)).toSeq.sorted)
    val equives = splitters.toSeq.map(s => s.sliding(2).map(x =>
      nums.slice(x(0), x(1))
    ).toSet)
    val flattened = ObservationalEquivalence.flattenIntersectConclusions(equives)
    val allNums = equives.flatten.flatten
    val flattenedMap = flattened.flatMap(s => s.map(x => x -> s)).toMap
    allNums foreach (x => {
      val fromEquives = equives.map(_.find(_.contains(x)).get).reduce((s1, s2) => s1.intersect(s2))
      fromEquives shouldEqual flattenedMap(x)
    })
  }

  test("test specific flatten intersection") {
    checkIntersectionFlatten(3, Set(Set(0), Set(1)))
  }

  test("test flatten intersection doesn't miss sets") {
    forAll (maxDiscardedFactor(100.0), SizeRange(10)) { (numsLen: Byte, splitAt: Set[Set[Int]]) =>
      whenever(numsLen > 1 && numsLen < 30 && splitAt.nonEmpty && splitAt.forall(_.nonEmpty)) {
        checkIntersectionFlatten(numsLen, splitAt)
      }
    }
  }

  test("test all nodes are present in original graph (auto add anchors)") {
    val graph = Programs.empty
      .addTerm(parser.parseExpression("x + y + z"))
      .addTerm(parser.parseExpression("x :: l ++ l2"))
      .addTerm(parser.parseExpression("x :: l2"))
      .addTerm(parser.parseExpression("1 + z"))
    val updatedState = new ActionSearchState(graph, normalRules)
    val equives = new ObservationalEquivalence().getEquives(updatedState.deepCopy())
    equives._2.flatten shouldEqual updatedState.programs.queryGraph.nodes
  }
}
