package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.imply
import org.scalacheck.Prop.BooleanOperators

import scala.util.Random
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers {
  implicit val edgeCreator = Arbitrary(integerEdgesGen)
  implicit val graphCreator = Arbitrary(integerGraphGen)

  def checkRemoved(g: VocabularyHyperGraph[Int, Int], i: Int): Boolean = {
    val e = g.edges.toList(i)
    !g.removeEdge(e).edges.contains(e)
  }

  property("removes") {
    check(forAll { g: VocabularyHyperGraph[Int, Int] =>
      g.edges.nonEmpty ==> checkRemoved(g, Random.nextInt(g.edges.size)) && false
    })
  }

  property("remove non existant") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> (g.removeEdge(e) != null)
    })
  }

  property("add non existant works") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> g.addEdge(e).edges.contains(e)
    })
  }

  property("edges finds all that were added") {
    check(forAll { es: Seq[HyperEdge[Int, Int]] =>
      val g = new VocabularyHyperGraph[Int, Int]()
      for (e <- es) g.addEdge(e)
      es.toSet.toSeq.intersect(g.edges.toSeq).size == es.size
    })
  }

  property("add than remove than add") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> (g.edges.size + 1 == g.addEdge(e).edges.size &&
        g.edges.size-1 == g.removeEdge(e).edges.size && g.edges.size + 1 == g.addEdge(e).edges.size)
    })
  }
}
