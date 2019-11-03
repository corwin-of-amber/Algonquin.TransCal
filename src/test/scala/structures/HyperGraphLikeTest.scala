package structures

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, ParallelTestExecution, PropSpec}
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckPropertyChecks}

import scala.util.Random

trait HyperGraphLikeTest[Node,
                        EdgeType,
                        T <: HyperGraphLike[Node, EdgeType, T] with collection.Set[HyperEdge[Node, EdgeType]],
                        Pattern <: HyperGraphLike.HyperGraphPattern[Node, EdgeType, Int, Pattern] with collection.Set[HyperGraphLike.HyperEdgePattern[Node,EdgeType,Int]]]
    extends PropSpec with Matchers with ScalaCheckPropertyChecks with ParallelTestExecution  {
  implicit def edgeCreator: Arbitrary[HyperEdge[Node, EdgeType]]
  implicit def graphCreator: Arbitrary[T]
  def grapher(es: Set[HyperEdge[Node, EdgeType]]): T
  def patterner(es: Set[HyperGraphLike.HyperEdgePattern[Node, EdgeType, Int]]): Pattern

  def checkRemoved(g: T, i: Int): Boolean = {
    val e = g.edges.toList(i)
    !(g - e).edges.contains(e)
  }

  property("removes") {
    forAll { g: T => whenever(g.edges.nonEmpty) {
      checkRemoved(g, Random.nextInt(g.edges.size)) shouldEqual true
    }}
  }

  property("remove non existant") {
    forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      whenever(!g.edges.contains(e)) {
        ((g - e) != null) && ((g - e).edges == g.edges) shouldEqual true
      }
    }
  }

  property("add non existant works") {
    forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      whenever(!g.edges.contains(e) && g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType)) {
        (g + e).edges should contain(e)
      }
    }
  }

  property("add edge with empty source") {
    forAll { e: HyperEdge[Node, EdgeType] =>
      val e1 = HyperEdge(e.target, e.edgeType, List.empty, EmptyMetadata)
      val g = grapher(Set.empty)
      (g + e1).edges should contain (e1)
    }
  }

  property("edges finds all that were added") {
    forAll ( maxDiscardedFactor(500.0)) { es: Set[HyperEdge[Node, EdgeType]] => whenever(es.forall(e => es.forall(e1 => e == e1 || e1.sources != e.sources || e1.edgeType != e.edgeType))) {
      val g = grapher(es)
      es.toSeq.intersect(g.edges.toSeq).size shouldEqual es.size
    }}
  }

  property("add than remove than add") {
    forAll { (g: T, e: HyperEdge[Node, EdgeType]) =>
      whenever(g.edges.forall(e1 => e1.sources != e.sources || e1.edgeType != e.edgeType)) {
        val gAdded = g + e
        val gRemoved = gAdded - e
        val gAdded2 = gRemoved + e
        g.edges.size + 1 shouldEqual gAdded.edges.size
        gAdded.edges.size - 1 shouldEqual gRemoved.edges.size
        gRemoved.edges.size + 1 shouldEqual gAdded2.edges.size
      }
    }
  }

  property("find by type returns all") {
    forAll { g: T =>
      g.map(_.edgeType).foreach(et => g.edges.filter(_.edgeType == et) shouldEqual g.findEdges(et))
    }
  }

  property("merge any different node returns graph without merged node") {
    forAll { g: T =>
      whenever(g.nodes.size > 1) {
        val first = g.nodes.head
        val second = g.nodes.last
        val gMerged = g.mergeNodes(first, second)
        val found = gMerged.findInNodes(second)
        gMerged.nodes should contain (first)
        (first == second || !gMerged.nodes.contains(second)) shouldEqual true
        found shouldBe empty
      }
    }
  }

  property("merge same node returns graph without merged node") {
    forAll { g: T =>
      whenever(g.nodes.nonEmpty) {
        val first = g.nodes.head
        val second = g.nodes.head
        val gMerged = g.mergeNodes(first, second)
        gMerged.nodes should contain (first)
        (first == second || !gMerged.nodes.contains(second)) shouldEqual true
      }
    }
  }

  property("merge nodes renames edges") {
    forAll { g: T =>
      whenever((g.size > 1) && (g.edges.head.target != g.edges.tail.head.target)) {
        g.edges.toList match {
          case source :: toChange :: _ =>
            val gMerged = g.mergeNodes(source.target, toChange.target)
            val found1 = gMerged.findRegex(HyperEdge(Explicit(toChange.target), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            val found2 = gMerged.findRegex(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(toChange.target), Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            gMerged.edges should not contain (toChange)
            gMerged.edges.exists(x => x.target == source.target && x.sources == toChange.sources.map({ x =>
              if (x == toChange.target) source.target
              else x
            }) && x.edgeType == toChange.edgeType) shouldEqual true
            (found1 ++ found2) shouldBe empty
        }
      }
    }
  }

  property("find edge by target equal to empty pattern except edge type") {
    forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      whenever(es.size > 1) {
        val g = grapher(es)
        es.foreach(e =>
          g.findEdges(e.edgeType).filter(_.sources.size == e.sources.size) shouldEqual
            g.findRegexHyperEdges(HyperEdge(Ignored(), Explicit(e.edgeType), e.sources.map(_ => Ignored()), EmptyMetadata)))
      }
    }
  }

  property("find all empty should find all edges by sources length") {
    forAll { g: T =>
      whenever(g.size > 1) {
        0 to 8 foreach  { i =>
          val pat = HyperEdge(Ignored(), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
          g.findRegex(pat).size shouldEqual g.count(_.sources.length == i)
        }
      }
    }
  }

  property("find all by target") {
    forAll { g: T =>
      whenever(g.size > 1) {
        g.map(_.target) foreach  { t =>
          0 to 8 foreach  { i =>
            val pat = HyperEdge(Explicit(t), Ignored(), Seq.fill(i)(Ignored()), EmptyMetadata)
            g.findRegex(pat).size shouldEqual g.count(e => e.sources.length == i && e.target == t)
          }
        }
      }
    }
  }

  property("find graph finds itself") {
    forAll { g: T =>
      val pg = patterner(g.edges map (e => new HyperGraphLike.HyperEdgePattern(Explicit(e.target), Explicit(e.edgeType), e.sources.map(Explicit[Node, Int]), EmptyMetadata)))
      g.findSubgraph[Int, Pattern](pg.asInstanceOf[Pattern]).size shouldEqual 1
    }
  }

  property("find graph finds edges by type") {
    forAll { g: T =>
      whenever(g.nonEmpty) {
        val pattern =
          g.head.copy(target = Explicit(g.head.target), edgeType = Hole(0), sources = g.head.sources.map(Explicit[Node, Int]))
        g.findRegexHyperEdges(pattern) should contain (g.head)
      }
    }
  }

  property("find with one ignore finds something") {
    forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      whenever(es.exists(_.sources.size == 1)) {
        val g = grapher(es)
        val pattern = HyperEdge(Hole(0), Hole(1), List(Ignored()), EmptyMetadata)
        g.findRegex(pattern) should not be empty
      }
    }
  }

  property("find by ignore regex finds all") {
    forAll { g: T =>
      whenever(g.exists(_.sources.size == 1)) {
        val pattern = HyperEdge(Hole(0), Hole(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
        g.findRegexHyperEdges(pattern) shouldEqual g.edges
      }
    }
  }

  property("find graph finds edge and replacer") {
    forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      whenever(es.exists(usedEdge => es.exists(usingEdge => usingEdge.sources.contains(usedEdge.target)))) {
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
        }) shouldEqual true
      }
    }
  }

  property("Find subgraph with and without merge returns same maps") {
    forAll { es: Set[HyperEdge[Node, EdgeType]] =>
      whenever(es.nonEmpty) {
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
        }) shouldEqual true
      }
    }
  }

  property("test FindInSources works correctly vs naive implementation") {
    forAll{ g: T => whenever(g.nonEmpty && g.head.sources.nonEmpty) {
      val s = g.head.sources.head
      g.edges.filter(_.sources.contains(s)) shouldEqual g.findInSources(s)
    }}
  }

  property("test findByTarget works correctly vs naive implementation") {
    forAll{ g: T => whenever(g.nonEmpty) {
      val s = g.head.target
      g.edges.filter(_.target == s) shouldEqual g.findByTarget(s)
    }}
  }

  property("test FindByEdgeType works correctly vs naive implementation") {
    forAll{ g: T => whenever(g.nonEmpty) {
      val s = g.head.edgeType
      g.edges.filter(_.edgeType == s) shouldEqual  g.findByEdgeType(s)
    }}
  }

  property("test nodes and edges agree on nodes in graph") {
    forAll{ g: T => {
      g.nodes shouldEqual g.edges.flatMap(e => e.target +: e.sources)
    }}
  }

  property("test FindInNodes works correctly vs naive implementation") {
    forAll{ g: T => whenever(g.nonEmpty) {
      val s = g.nodes.head
      g.edges.filter(e => e.target == s || e.sources.contains(s)) shouldEqual g.findInNodes(s)
    }}
  }
}
