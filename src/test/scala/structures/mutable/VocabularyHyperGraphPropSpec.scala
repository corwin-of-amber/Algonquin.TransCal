package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
import structures.HyperGraphManyWithOrderToOneLike.{Explicit, HyperEdge, Ignored, Item}

import scala.util.Random


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
      val g = grapher(es)
      es.toSeq.intersect(g.edges.toSeq).size == es.size
    })
  }

  property("add than remove than add") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> (g.edges.size + 1 == g.addEdge(e).edges.size &&
        g.edges.size - 1 == g.removeEdge(e).edges.size && g.edges.size + 1 == g.addEdge(e).edges.size)
    })
  }

  property("find by type returns all") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      es.map(_.edgeType).forall(et => es.filter(_.edgeType == et) == g.findEdges(et))
    })
  }

  property("test cycles using graph library") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val hg = grapher(es)
      val graph = Graph()
      val newEdges = es.flatMap(he => he.sources.map(DiEdge(_, he.target)))
      val graph2 = graph ++ newEdges
      hg.cycles == graph2.isCyclic
    })
  }

  property("merge any different node returns graph without merged node") {
    check(forAll { g: VocabularyHyperGraph[Int, Int] =>
      g.nodes.nonEmpty ==> {
        val first = g.nodes.toList(Random.nextInt(g.nodes.size))
        val second = g.nodes.toList(Random.nextInt(g.nodes.size))
        g.mergeNodes(first, second)
        g.nodes.contains(first) && (first == second || !g.nodes.contains(second))
      }
    })
  }

  property("merge nodes renames edges") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        val toChange = es.toList(1)
        val source = es.toList(0)
        g.mergeNodes(source.target, toChange.target)
        !g.edges.contains(toChange) && g.edges.count(x => x.target == source.target && x.sources == toChange.sources.map({x =>
          if (x == toChange.target) source.target
          else x
        }) && x.edgeType == toChange.edgeType) == 1
      }
    })
  }

  property("find edge by target equal to empty pattern except target") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        es.forall(e => g.findEdges(e.target) == g.find(HyperEdge[Item[Int, Int], Item[Int, Int]](Explicit(e.target), Ignored(), e.sources.map(_ => Ignored[Int, Int]()))))
      }
    })
  }
}
