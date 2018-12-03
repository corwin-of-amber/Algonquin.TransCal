package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.imply
import org.scalacheck.Prop.BooleanOperators

import scala.util.Random
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
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
      g.edges.nonEmpty ==> checkRemoved(g, Random.nextInt(g.edges.size))
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
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = new VocabularyHyperGraph[Int, Int]()
      for (e <- es) g.addEdge(e)
      es.toSeq.intersect(g.edges.toSeq).size == es.size
    })
  }

  property("add than remove than add") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> (g.edges.size + 1 == g.addEdge(e).edges.size &&
        g.edges.size-1 == g.removeEdge(e).edges.size && g.edges.size + 1 == g.addEdge(e).edges.size)
    })
  }

  property("find by type returns all") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = new VocabularyHyperGraph[Int, Int]()
      for (e <- es) g.addEdge(e)
      es.map(_.edgeType).forall(et => es.filter(_.edgeType == et) == g.findEdges(et))
    })
  }

  property("test cycles using graph library") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val hg = new VocabularyHyperGraph[Int, Int]()
      var graph = Graph()
      for (e <- es) {
        hg.addEdge(e)
      }
      val newEdges = es.flatMap(he => he.sources.map(DiEdge(_, he.target)))
      val graph2 = graph ++ newEdges
      hg.cycles == graph2.isCyclic
    })
  }
}
