package synthesis.actions.operators

import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import synthesis.actions.ActionSearchState
import synthesis.rewrites.Template.ReferenceTerm
import synthesis.{AssociativeRewriteRulesDB, HyperTermId, Programs, SimpleRewriteRulesDB}
import transcallang.{AnnotatedTree, Language, TranscalParser}

class ObservationalEquivalenceTest extends FunSuite with ScalaCheckPropertyChecks with Matchers {
  val parser = new TranscalParser
  test("testFromTerms") {
    val eqiuivalentTerms = Seq(
      "x + y",
      "y + x",
      s"x :: y :: ${Language.nilId.literal}",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal})",
      s"${Language.nilId.literal} ++ (x :: y :: ${Language.nilId.literal}) ++ ${Language.nilId.literal}"
    ).map(parser.parseExpression)
    val res = new ObservationalEquivalence(3).fromTerms(eqiuivalentTerms, AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules)
    val expected = Set(eqiuivalentTerms.take(2).toSet, eqiuivalentTerms.drop(2).toSet)
    expected shouldEqual res
  }

  test("testGetEquives") {
    val terms = "(x + y + z),(z + x + y),(j + k + z),(j + k),(k + j)".split(",").map(parser.parseExpression)
    val programs = Programs.empty.addTerm(AnnotatedTree.withoutAnnotations(Language.limitedAndCondBuilderId, terms))
    val ids = terms.map(t => {
      val (graph, root) = Programs.destructPatternsWithRoots(Seq(t)).head
      programs.hyperGraph.findSubgraph[Int](graph).head._1(root.asInstanceOf[ReferenceTerm[HyperTermId]].id)
    }).toSeq
    val res = new ObservationalEquivalence(3).getEquives(ActionSearchState(Programs(programs.hyperGraph ++ ids.map(ObservationalEquivalence.createAnchor)), AssociativeRewriteRulesDB.rewriteRules ++ SimpleRewriteRulesDB.rewriteRules))
    Set(Set(ids take 2: _*), Set(ids.slice(3, 5): _*), Set(ids(2))) shouldEqual res
  }

  test("test flatten union doesn't miss sets") {
    forAll { (nums: Set[Int], splitAt: Set[Set[Int]]) =>
      whenever(nums.nonEmpty && splitAt.nonEmpty && splitAt.forall(_.nonEmpty)) {
        val splitters = splitAt.map(s => (s.map(math.abs(_) % nums.size) + 0 + (nums.size - 1)).toSeq.sorted)
        val equives = splitters.toSeq.map(s => s.sliding(2).toSeq.map({ case x => nums.slice(x(0), x(1))}).toSet)
        val flattened = ObservationalEquivalence.flattenUnionConclusions(equives)
        val allNums = equives.flatten.flatten
        val flattenedMap = flattened.flatMap(s => s.map(x => x -> s)).toMap
        allNums foreach (x => {
          val fromEquives = equives.map(s => s.find(_ contains x).getOrElse(Set.empty))
          flattenedMap(x) foreach (y => fromEquives.exists(_.contains(y)) shouldEqual true)
          fromEquives foreach (s => flattenedMap(x) should contain allElementsOf s)
        })
      }
    }
  }

  test("test flatten intersection doesn't miss sets") {
    forAll { (nums: Set[Int], splitAt: Set[Set[Int]]) =>
      whenever(nums.nonEmpty && splitAt.nonEmpty && splitAt.forall(_.nonEmpty)) {
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
    }
  }
}
