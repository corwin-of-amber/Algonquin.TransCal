package structures

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers

import scala.util.Random

trait HyperGraphLikeTest[Node,
                        EdgeType,
                        T <: HyperGraphLike[Node, EdgeType, T] with collection.Set[HyperEdge[Node, EdgeType]],
                        Pattern <: HyperGraphLike.HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperGraphLike.HyperEdgePattern[Node,EdgeType,Int]]]
    extends PropSpec with Checkers with Matchers {
  implicit def edgeCreator: Arbitrary[HyperEdge[Node, EdgeType]]
  implicit def graphCreator: Arbitrary[T]
  def grapher(es: Set[HyperEdge[Node, EdgeType]]): T
  def patterner(es: Set[HyperGraphLike.HyperEdgePattern[Node, EdgeType, Int]]): Pattern

  def checkRemoved(g: T, i: Int): Boolean = {
    val e = g.edges.toList(i)
    !(g - e).edges.contains(e)
  }

  property("removes") {
    check(forAll { g: T => (g.edges.nonEmpty) ==>
      checkRemoved(g, Random.nextInt(g.edges.size))
    })
  }

  property("remove non existant") {
    check(forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      !g.edges.contains(e) ==> ((g - e) != null) && ((g - e).edges == g.edges)
    })
  }

  property("add non existant works") {
    check(forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      (!g.edges.contains(e) && g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType)) ==>
        (g + e).edges.contains(e)
    })
  }

  property("add edge with empty source") {
    check(forAll { e: HyperEdge[Node, EdgeType] =>
      val e1 = HyperEdge(e.target, e.edgeType, List.empty, EmptyMetadata)
      val g = grapher(Set.empty)
      (g + e1).edges.contains(e1)
    })
  }

  property("edges finds all that were added") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] => (es.forall(e => es.forall(e1 => e == e1 || e1.sources != e.sources || e1.edgeType != e.edgeType))) ==> {
      val g = grapher(es)
      es.toSeq.intersect(g.edges.toSeq).size == es.size
    }})
  }

  property("add than remove than add") {
    check(forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType) ==> {
        val gAdded = g + e
        val gRemoved = gAdded - e
        val gAdded2 = gRemoved + e
        g.edges.size + 1 == gAdded.edges.size && gAdded.edges.size - 1 == gRemoved.edges.size && gRemoved.edges.size + 1 == gAdded2.edges.size
      }
    })
  }

  property("find by type returns all") {
    check(forAll { g: T =>
      g.map(_.edgeType).forall(et => g.edges.filter(_.edgeType == et) == g.findEdges(et))
    })
  }

  property("merge any different node returns graph without merged node") {
    check(forAll { g: T =>
      (g.nodes.size > 1) ==> {
        val first = g.nodes.head
        val second = g.nodes.last
        val gMerged = g.mergeNodes(first, second)
        val found = gMerged.findInNodes(second)
        gMerged.nodes.contains(first) && (first == second || !gMerged.nodes.contains(second)) && found.isEmpty
      }
    })
  }

  property("merge same node returns graph without merged node") {
    check(forAll { g: T =>
      g.nodes.nonEmpty ==> {
        val first = g.nodes.head
        val second = g.nodes.head
        val gMerged = g.mergeNodes(first, second)
        gMerged.nodes.contains(first) && (first == second || !gMerged.nodes.contains(second))
      }
    })
  }

  property("merge nodes renames edges") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
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
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      (es.size > 1) ==> {
        val g = grapher(es)
        es.forall(e => g.findEdges(e.edgeType).filter(_.sources.size == e.sources.size) == g.findRegexHyperEdges(HyperEdge(Ignored(), Explicit(e.edgeType), e.sources.map(_ => Ignored()), EmptyMetadata)))
      }
    })
  }

  property("find all empty should find all edges by sources length") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
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
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
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
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      val g = grapher(es)
      val pg = patterner(es map (e => HyperEdge(Explicit(e.target), Explicit(e.edgeType), e.sources.map(Explicit[Node, Int]), EmptyMetadata)))
      g.findSubgraph[Int, Pattern](pg.asInstanceOf[Pattern]).size == 1
    })
  }

  property("find graph finds edges by type") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      es.nonEmpty ==> {
        val g = grapher(es)
        val pattern =
          es.head.copy(target = Explicit(es.head.target), edgeType = Hole(0), sources = es.head.sources.map(Explicit[Node, Int]))
        g.findRegexHyperEdges(pattern).contains(es.head)
      }
    })
  }

  property("find with one ignore finds something") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      es.exists(_.sources.size == 1) ==> {
        val g = grapher(es)
        val pattern = HyperEdge(Hole(0), Hole(1), List(Ignored()), EmptyMetadata)
        g.findRegex(pattern).nonEmpty
      }
    })
  }

  property("find by ignore regex finds all") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      es.exists(_.sources.size == 1) ==> {
        val g = grapher(es)
        val pattern = HyperEdge(Hole(0), Hole(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
        g.findRegexHyperEdges(pattern) == es
      }
    })
  }

  property("find graph finds edge and replacer") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      es.exists(usedEdge => es.exists(usingEdge => usingEdge.sources.contains(usedEdge.target))) ==> {
        val g = grapher(es)
        es.exists(e => {
          e.sources.indices exists { i =>
            val pg = patterner(Set(
              HyperEdge(Hole(0), Ignored(), Seq(Repetition.rep0[Node, Int](Int.MaxValue, Stream.continually(Ignored())).get), EmptyMetadata),
              HyperEdge(Ignored(), Ignored(), e.sources.zipWithIndex.map(si => if (si._2 == i) Hole(0) else Ignored()), EmptyMetadata)
            ))
            val results = g.findSubgraph[Int, Pattern](pg.asInstanceOf[Pattern]).headOption.map(_._1)
            results.nonEmpty && results.get(0) == e.sources(i)
          }
        })
      }
    })
  }

  property("Find subgraph with and without merge returns same maps") {
    check(forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      es.nonEmpty ==> {
        val subgraph = Random.shuffle(es).take(Random.nextInt(es.size))
        val asHoles: Map[Node, Int] = {
          val creator = Stream.from(0).iterator
          subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
        }
        val pattern: Pattern = {
          val pEdges = subgraph.map(e => HyperEdge[Item[Node, Int], Item[EdgeType, Int]](Hole[Node, Int](asHoles(e.target)), Explicit[EdgeType,Int](e.edgeType), e.sources.map(x => Hole[Node, Int](asHoles(x))), e.metadata))
          patterner(pEdges)
        }
        val graph = grapher(es)
        val maps = graph.findSubgraph[Int, Pattern](pattern)
        val holes = pattern.nodes.collect({case e: Hole[Node, Int] => e})
        holes.forall(h => {
          maps.groupBy(_._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int, Pattern](pattern.mergeNodes(Explicit[Node, Int](vAndMaps._1), h))
          })
        })
      }
    })
  }

  property("test FindInSources works correctly vs naive implementation") {
    check(forAll{ (g: T) => (g.nonEmpty && g.head.sources.nonEmpty) ==> {
      val s = g.head.sources.head
      g.edges.filter(_.sources.contains(s)) == g.findInSources(s)
    }})
  }

  property("test findByTarget works correctly vs naive implementation") {
    check(forAll{ (g: T) => g.nonEmpty ==> {
      val s = g.head.target
      g.edges.filter(_.target == s) == g.findByTarget(s)
    }})
  }

  property("test FindByEdgeType works correctly vs naive implementation") {
    check(forAll{ (g: T) => g.nonEmpty ==> {
      val s = g.head.edgeType
      g.edges.filter(_.edgeType == s) == g.findByEdgeType(s)
    }})
  }

  property("test nodes and edges agree on nodes in graph") {
    check(forAll{ (g: T) => {
      g.nodes == g.edges.flatMap(e => e.target +: e.sources)
    }})
  }

  property("test FindInNodes works correctly vs naive implementation") {
    check(forAll{ (g: T) => g.nonEmpty ==> {
      val s = g.nodes.head
      g.edges.filter(e => e.target == s || e.sources.contains(s)) == g.findInNodes(s)
    }})
  }
}
