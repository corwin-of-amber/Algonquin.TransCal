package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures._

import scala.util.Random


class VocabularyHyperGraphPropSpec extends PropSpec with Checkers with Matchers {
  private implicit val edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  private implicit val graphCreator: Arbitrary[VocabularyHyperGraph[Int, Int]] = Arbitrary(integerGraphGen)

  def checkRemoved(g: VocabularyHyperGraph[Int, Int], i: Int): Boolean = {
    val e = g.edges.toList(i)
    !(g - e).edges.contains(e)
  }

  property("all constructor") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      new VocabularyHyperGraph(es).edges == es && VocabularyHyperGraph(es.toSeq: _*).edges == es
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
      (g.nodes.size > 1) ==> {
        val first = g.nodes.head
        val second = g.nodes.last
        val gMerged = g.mergeNodes(first, second)
        val found1 = gMerged.findRegex(HyperEdge(Explicit(second), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
        val found2 = gMerged.findRegex(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(second), Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
        gMerged.nodes.contains(first) && (first == second || !gMerged.nodes.contains(second)) && (found1 ++ found2).isEmpty
      }
    })
  }

  property("merge same node returns graph without merged node") {
    check(forAll { g: VocabularyHyperGraph[Int, Int] =>
      g.nodes.nonEmpty ==> {
        val first = g.nodes.head
        val second = g.nodes.head
        val gMerged = g.mergeNodes(first, second)
        gMerged.nodes.contains(first) && (first == second || !gMerged.nodes.contains(second))
      }
    })
  }

  property("merge nodes renames edges") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      ((es.size > 1) && (es.head.target != es.tail.head.target)) ==> {
        val g = grapher(es)
        es.toList match {
          case source :: toChange :: _ =>
            val gMerged = g.mergeNodes(source.target, toChange.target)
            val found1 = gMerged.findRegex(HyperEdge(Explicit(toChange.target), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            val found2 = gMerged.findRegex(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(toChange.target), Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            !gMerged.edges.contains(toChange) && gMerged.edges.exists(x => x.target == source.target && x.sources == toChange.sources.map({ x =>
              if (x == toChange.target) source.target
              else x
            }) && x.edgeType == toChange.edgeType) && (found1 ++ found2).isEmpty
        }
      }
    })
  }

  property("find edge by target equal to empty pattern except edge type") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        es.forall(e => g.findEdges(e.edgeType).filter(_.sources.size == e.sources.size) == g.findRegexHyperEdges(HyperEdge(Ignored(), Explicit(e.edgeType), e.sources.map(_ => Ignored()), EmptyMetadata)))
      }
    })
  }

  property("find all empty should find all edges by sources length") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        0 to 8 forall { i =>
          val pat = HyperEdge(Ignored(), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
          g.findRegex(pat).size == es.count(_.sources.length == i)
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
            g.findRegex(pat).size == es.count(e => e.sources.length == i && e.target == t)
          }
        }
      }
    })
  }

  property("find graph finds itself") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val pg = grapher[Item[Int, Int], Item[Int, Int]](es map (e => HyperEdge(Explicit(e.target), Explicit(e.edgeType), e.sources.map(Explicit[Int, Int]), EmptyMetadata)))
      g.findSubgraph[Int, VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]](pg).size == 1
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
        g.findSubgraph[Int, VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]](pg).isEmpty
      }
      )
    })
  }

  property("find graph finds edges by type") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.nonEmpty ==> {
        val g = grapher(es)
        val pattern =
          es.head.copy(target = Explicit(es.head.target), edgeType = Hole(0), sources = es.head.sources.map(Explicit[Int, Int]))
        g.findRegexHyperEdges(pattern).contains(es.head)
      }
    })
  }

  property("find with one ignore finds something") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.exists(_.sources.size == 1) ==> {
        val g = grapher(es)
        val pattern = HyperEdge(Hole(0), Hole(1), List(Ignored()), EmptyMetadata)
        g.findRegex(pattern).nonEmpty
      }
    })
  }

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
    val found = graph.findRegexHyperEdges(pattern)
    val edges = graph.edges
    check(found == edges)
  }

  property("find by ignore regex finds all") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.exists(_.sources.size == 1) ==> {
        val g = grapher(es)
        val pattern = HyperEdge(Hole(0), Hole(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
        g.findRegexHyperEdges(pattern) == es
      }
    })
  }

  property("find graph finds edge and replacer") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.exists(usedEdge => es.exists(usingEdge => usingEdge.sources.contains(usedEdge.target))) ==> {
        val g = grapher(es)
        es.exists(e => {
          e.sources.indices exists { i =>
            val pg = grapher[Item[Int, Int], Item[Int, Int]](Set(
              HyperEdge(Hole(0), Ignored(), Seq(Repetition.rep0[Int, Int](Int.MaxValue, Stream.continually(Ignored())).get), EmptyMetadata),
              HyperEdge(Ignored(), Ignored(), e.sources.zipWithIndex.map(si => if (si._2 == i) Hole(0) else Ignored()), EmptyMetadata)
            ))
            val results = g.findSubgraph[Int, VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]](pg).headOption.map(_._1)
            results.nonEmpty && results.get(0) == e.sources(i)
          }
        })
      }
    })
  }

  property("Find subgraph with and without merge returns same maps") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.nonEmpty ==> {
        val subgraph = Random.shuffle(es).take(Random.nextInt(es.size))
        val asHoles: Map[Int, Int] = {
          val creator = Stream.from(0).iterator
          subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
        }
        val pattern: structures.immutable.HyperGraph[Item[Int, Int], Item[Int, Int]] = {
          val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
          structures.immutable.HyperGraph(pEdges.toSeq: _*)
        }
        val graph = HyperGraph(es.toSeq: _*)
        val maps = graph.findSubgraph[Int](pattern)
        val holes = pattern.nodes.filter(_.isInstanceOf[Hole[Int, Int]]).map(_.asInstanceOf[Hole[Int, Int]])
        holes.forall(h => {
          maps.groupBy(_._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
          })
        })
      }
    })
  }

  property("test FindInSources works correctly vs naive implementation") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => (g.nonEmpty && g.head.sources.nonEmpty) ==> {
      val s = g.head.sources.head
      g.edges.filter(_.sources.contains(s)) == g.findInSources(s)
    }})
  }

  property("test findByTarget works correctly vs naive implementation") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val s = g.head.target
      g.edges.filter(_.target == s) == g.findByTarget(s)
    }})
  }

  property("test FindByEdgeType works correctly vs naive implementation") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val s = g.head.edgeType
      g.edges.filter(_.edgeType == s) == g.findByEdgeType(s)
    }})
  }

  property("test nodes and edges agree on nodes in graph") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => {
      g.nodes == g.edges.flatMap(e => e.target +: e.sources)
    }})
  }

  property("test FindInNodes works correctly vs naive implementation") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val s = g.head.edgeType
      g.edges.filter(e => e.target == s || e.sources.contains(s)) == g.findInNodes(s)
    }})
  }
}
