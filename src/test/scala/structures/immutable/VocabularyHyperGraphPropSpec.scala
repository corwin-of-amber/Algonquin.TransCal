package structures.immutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures._

import scala.util.Random


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers {
  implicit val edgeCreator = Arbitrary(integerEdgesGen)
  implicit val graphCreator = Arbitrary(integerGraphGen)

  def checkRemoved(g: VocabularyHyperGraph[Int, Int], i: Int): Boolean = {
    val e = g.edges.toList(i)
    !(g - e).edges.contains(e)
  }

  property("all constructor") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      new VocabularyHyperGraph(es).edges == es && VocabularyHyperGraph(es.toSeq:_*).edges == es
    })
  }

  property("removes") {
    check(forAll { g: VocabularyHyperGraph[Int, Int] =>
      g.edges.nonEmpty ==> checkRemoved(g, Random.nextInt(g.edges.size))
    })
  }

  property("remove non existant") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> ((g - e) != null) && ((g - e).edges == g.edges)
    })
  }

  property("add non existant works") {
    check(forAll { (g: VocabularyHyperGraph[Int, Int], e: HyperEdge[Int, Int]) =>
      !g.edges.contains(e) ==> (g + e).edges.contains(e)
    })
  }

  property("add edge with empty source") {
    check(forAll { e: HyperEdge[Int, Int] =>
      val e1 = HyperEdge(e.edgeType, e.target, List.empty, EmptyMetadata)
      (VocabularyHyperGraph.empty + e1).edges.contains(e1)
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
      !g.edges.contains(e) ==> {
        val gAdded = g + e
        val gRemoved = gAdded - e
        val gAdded2 = gRemoved + e
        g.edges.size + 1 == gAdded.edges.size && gAdded.edges.size - 1 == gRemoved.edges.size && gRemoved.edges.size + 1 == gAdded2.edges.size
      }
    })
  }

  property("find by type returns all") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      es.map(_.edgeType).forall(et => es.filter(_.edgeType == et) == g.findEdges(et))
    })
  }

  property("merge any different node returns graph without merged node") {
    check(forAll { g: VocabularyHyperGraph[Int, Int] =>
      g.nodes.nonEmpty ==> {
        val first = g.nodes.toList(Random.nextInt(g.nodes.size))
        val second = g.nodes.toList(Random.nextInt(g.nodes.size))
        val gMerged = g.mergeNodes(first, second)
        gMerged.nodes.contains(first) && (first == second || !gMerged.nodes.contains(second))
      }
    })
  }

  property("merge nodes renames edges") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      ((es.size > 1) && (es.head.target != es.tail.head.target)) ==> {
        val g = grapher(es)
        val toChange = es.toList(1)
        val source = es.toList(0)
        val gMerged = g.mergeNodes(source.target, toChange.target)
        !gMerged.edges.contains(toChange) && gMerged.edges.exists(x => x.target == source.target && x.sources == toChange.sources.map({ x =>
          if (x == toChange.target) source.target
          else x
        }) && x.edgeType == toChange.edgeType)
      }
    })
  }

  property("find edge by target equal to empty pattern except edge type") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        es.forall(e => g.findEdges(e.edgeType).filter(_.sources.size == e.sources.size) == g.find(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(e.edgeType), e.sources.map(_ => Ignored[Int, Int]()), EmptyMetadata)))
      }
    })
  }

  property("find all empty should find all edges by sources length") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        0 to 8 forall { i =>
          val pat = HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
          g.find(pat).size == es.count(_.sources.length == i)
        }
      }
    })
  }

  property("find all by target") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        es.map(_.target) forall { t =>
          0 to 8 forall { i =>
            val pat = HyperEdge[Item[Int, Int], Item[Int, Int]](Explicit(t), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
            g.find(pat).size == es.count(e => e.sources.length == i && e.target == t)
          }
        }
      }
    })
  }

  property("find graph finds itself") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val pg: VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] =
        grapher(es map (e => HyperEdge[Item[Int, Int], Item[Int, Int]](Explicit(e.target), Explicit(e.edgeType), e.sources.map(Explicit[Int, Int]), EmptyMetadata)))
      g.findSubgraph[Int](pg).size == 1
    })
  }

  property("find graph finds nothing") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored[Int, Int](), Explicit[Int, Int](800), Seq[Item[Int, Int]](Ignored[Int, Int](), Ignored[Int, Int]()), EmptyMetadata),
        HyperEdge[Item[Int, Int], Item[Int, Int]](Explicit[Int, Int](800), Ignored[Int, Int](), Seq[Item[Int, Int]](), EmptyMetadata),
        HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored[Int, Int](), Ignored[Int, Int](), Seq[Item[Int, Int]](Explicit[Int, Int](800)), EmptyMetadata)).map(Set(_))
      edges.forall(e => {
        val pg: VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] = grapher(e)
        g.findSubgraph[Int](pg).isEmpty
      }
      )
    })
  }

  property("find graph finds edge and replacer") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.exists(e => es.exists(e1 => e1.sources.contains(e.target))) ==> {
        val g = grapher(es)
        es.exists(e => {
          e.sources.indices exists { i =>
            0 until 10 exists { j =>
              val pg: VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] =
                grapher(Set(HyperEdge[Item[Int, Int], Item[Int, Int]](Hole(0), Ignored[Int, Int](), (0 until j).map(_ => Ignored[Int, Int]()), EmptyMetadata),
                  HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Ignored[Int, Int](), e.sources.zipWithIndex.map(si => if (si._2 == i) Hole[Int, Int](0) else Ignored[Int, Int]()), EmptyMetadata)))
              val results = g.findSubgraph[Int](pg)
              val resultsValues = results.map(t=> (t._1.values, t._2.values))
              val foundTarget = resultsValues.map(t => t._1 ++ t._2).forall(vals => vals.toList.contains(e.sources(i)))
              results.nonEmpty && foundTarget
            }
          }
        }
        )
      }
    })
  }
}
