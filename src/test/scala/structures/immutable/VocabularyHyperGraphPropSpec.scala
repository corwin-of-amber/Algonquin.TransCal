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
      (!g.edges.contains(e) && g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType)) ==>
        (g + e).edges.contains(e)
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
      g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType) ==> {
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
        es.forall(e => g.findEdges(e.edgeType).filter(_.sources.size == e.sources.size) == g.find(HyperEdge(Ignored(), Explicit(e.edgeType), e.sources.map(_ => Ignored()), EmptyMetadata)))
      }
    })
  }

  property("find all empty should find all edges by sources length") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        0 to 8 forall { i =>
          val pat = HyperEdge(Ignored(), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
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
            val pat = HyperEdge(Explicit(t), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
            g.find(pat).size == es.count(e => e.sources.length == i && e.target == t)
          }
        }
      }
    })
  }

  property("find graph finds itself") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val pg = grapher[Item[Int, Int], Item[Int, Int]](es map (e => HyperEdge(Explicit(e.target), Explicit(e.edgeType), e.sources.map(Explicit[Int, Int]), EmptyMetadata)))
      g.findSubgraph[Int](pg).size == 1
    })
  }

  property("find graph finds nothing") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(800), Seq(Ignored(), Ignored()), EmptyMetadata),
        HyperEdge(Explicit(800), Ignored(), Seq(), EmptyMetadata),
        HyperEdge(Ignored(), Ignored(), Seq(Explicit(800)), EmptyMetadata)).map(Set(_))
      edges.forall(e => {
        val pg = grapher[Item[Int, Int], Item[Int, Int]](e)
        g.findSubgraph[Int](pg).isEmpty
      }
      )
    })
  }

  property("find graph finds edges by type") {
    check(forAll { es: Set[HyperEdge[Int, Int]] => es.nonEmpty ==> {
      val g = grapher(es)
      val pattern =
        es.head.copy(target = Explicit(es.head.target), edgeType = Hole(0), sources = es.head.sources.map(Explicit[Int, Int]))
      g.find(pattern).contains(es.head)
  }})}

  property("find with one ignore finds something") {
    check(forAll { es: Set[HyperEdge[Int, Int]] => es.exists(_.sources.size == 1) ==> {
      val g = grapher(es)
      val pattern = HyperEdge(Hole(0), Hole(1), List(Ignored()), EmptyMetadata)
      g.find(pattern).nonEmpty
    }})}

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
    val found = graph.find(pattern)
    val edges = graph.edges
    check(found == edges)
  }

  property("find by ignore regex finds all") {
    check(forAll { es: Set[HyperEdge[Int, Int]] => es.exists(_.sources.size == 1) ==> {
      val g = grapher(es)
      val pattern = HyperEdge(Hole(0), Hole(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
      g.find(pattern) == es
    }})}

  property("find graph finds edge and replacer") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.exists(e => es.exists(e1 => e1.sources.contains(e.target))) ==> {
        val g = grapher(es)
        es.exists(e => {
          e.sources.indices exists { i =>
            0 until 10 exists { j =>
              val pg = grapher[Item[Int, Int], Item[Int, Int]](Set(HyperEdge(Hole(0), Ignored(), (0 until j).map(_ => Ignored()), EmptyMetadata),
                  HyperEdge(Ignored(), Ignored(), e.sources.zipWithIndex.map(si => if (si._2 == i) Hole(0) else Ignored()), EmptyMetadata)))
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
