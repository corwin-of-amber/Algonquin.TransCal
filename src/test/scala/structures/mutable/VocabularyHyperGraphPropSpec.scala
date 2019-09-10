package structures.mutable

import transcallang.{Identifier, Language, TranscalParser}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatestplus.scalacheck.Checkers
import org.scalatest.{Matchers, PropSpec}
import structures._
import synthesis.rewrites.Template.{ExplicitTerm}
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}

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
        val gMerged = g.mergeNodesInPlace(first, second)
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
        val gMerged = g.mergeNodesInPlace(first, second)
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
            val gMerged = g.mergeNodesInPlace(source.target, toChange.target)
            val found1 = gMerged.findRegex(HyperEdge(Explicit(toChange.target), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            val found2 = gMerged.findRegex(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(toChange.target), Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
            val findChanged = gMerged.findRegex(HyperEdge(Explicit(source.target), Explicit(toChange.edgeType), toChange.sources.map(s => if (s == toChange.target) source.target else s).map(Explicit(_)), EmptyMetadata))
            val exists = gMerged.edges.exists(e => e.target == source.target && e.edgeType == toChange.edgeType && e.sources == toChange.sources.map(s => if (s == toChange.target) source.target else s))
            exists && (!gMerged.edges.contains(toChange)) && findChanged.nonEmpty && (found1 ++ found2).isEmpty
        }
      }
    })
  }

  property("Specific - merge nodes renames edges") {
    val es = Set(HyperEdge(29,4,Vector(46, 7, 0, 37, 12),EmptyMetadata), HyperEdge(32,20,Vector(2, 34),EmptyMetadata), HyperEdge(43,13,Vector(8, 32, 42, 38),EmptyMetadata))
    val g = grapher(es)
    es.toList match {
      case source :: toChange :: _ =>
        val gMerged = g.mergeNodesInPlace(source.target, toChange.target)
        val found1 = gMerged.findRegex(HyperEdge(Explicit(toChange.target), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
        val found2 = gMerged.findRegex(HyperEdge(Ignored(), Ignored(), Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(toChange.target), Repetition.rep0(Int.MaxValue, Ignored()).get), EmptyMetadata))
        val findChanged = gMerged.findRegex(HyperEdge(Explicit(source.target), Explicit(toChange.edgeType), toChange.sources.map(s => if (s == toChange.target) source.target else s).map(Explicit(_)), EmptyMetadata))
        val exists = gMerged.edges.exists(e => e.target == source.target && e.edgeType == toChange.edgeType && e.sources == toChange.sources.map(s => if (s == toChange.target) source.target else s))
        check(exists && (!gMerged.edges.contains(toChange)) && findChanged.nonEmpty && (found1 ++ found2).isEmpty)
    }
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

  property("Compactions works correctly on mutliple swithces of same var") {
    val graphs = Programs.destructPatterns(new TranscalParser().apply("(?x ≤ ?y) ||> min(x, y) >> id x").subtrees)
    check(graphs(1).edges.head.sources.head == graphs.head.edges.find(_.edgeType.asInstanceOf[ExplicitTerm[HyperTermIdentifier]].value.identifier.literal == "≤").get.sources.head)
    check(graphs(1).edges.head.sources.head == graphs.head.edges.find(_.edgeType.asInstanceOf[ExplicitTerm[HyperTermIdentifier]].value.identifier.literal == "min").get.sources.head)
  }

  property("Compation works correctly when adding precondition after creation") {
    val term = new TranscalParser().apply("1 -> min(a, b)").subtrees(1)
    val tempGraph = Programs.destruct(term)
    val graph = tempGraph.++(Set(
      HyperEdge(HyperTermId(100), HyperTermIdentifier(Identifier("a")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(101), HyperTermIdentifier(Identifier("b")), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(Language.trueId), Seq.empty, EmptyMetadata),
      HyperEdge(HyperTermId(103), HyperTermIdentifier(Identifier("≤")), List(HyperTermId(100), HyperTermId(101)), EmptyMetadata)
    ))
    val aTerm = graph.findEdges(HyperTermIdentifier(Identifier("a"))).head.target
    val originalATerm = tempGraph.findEdges(HyperTermIdentifier(Identifier("a"))).head.target
    val newATerm = HyperTermId(100)
    val bTerm = graph.findEdges(HyperTermIdentifier(Identifier("b"))).head.target
    val originalBTerm = tempGraph.findEdges(HyperTermIdentifier(Identifier("b"))).head.target
    val newBTerm = HyperTermId(101)
    if (aTerm == originalATerm)
      graph.nodes should not contain newATerm
    else if (aTerm == newATerm)
      graph.nodes should not contain originalATerm
    else fail()
    if (bTerm == originalBTerm)
      graph.nodes should not contain newBTerm
    else if (bTerm == newBTerm)
      graph.nodes should not contain originalBTerm
    else fail()
  }

  property("If graphs are equal then hash is equal") {
    // Not necessarily true because of compaction
    check(forAll { es: Set[HyperEdge[Int, Int]] => HyperGraph(es.toSeq: _*).hashCode == HyperGraph(es.toSeq: _*).hashCode() })
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
        val holes = pattern.nodes.collect({ case h: Hole[Int, Int] => h })
        holes.forall(h => {
          maps.groupBy(m => m._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
          })
        })
      }
    })
  }

  property("Find Compact subgraph with and without merge returns same maps") {
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

        val graph = CompactHyperGraph(es.toSeq: _*)
        val maps = graph.findSubgraph[Int](pattern)
        val holes = pattern.nodes.collect({ case h: Hole[Int, Int] => h })

        holes.forall(h => {
          maps.groupBy(_._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
          })
        })
      }
    })
  }

  property("Find Versioned subgraph with and without merge returns same maps") {
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

        val graph = VersionedHyperGraph(es.toSeq: _*)
        val maps = graph.findSubgraph[Int](pattern)
        val holes = pattern.nodes.collect({ case h: Hole[Int, Int] => h })
        holes.forall(h => {
          maps.groupBy(_._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
          })
        })
      }
    })
  }

  property("Specific - Find Versioned subgraph with and without merge returns same maps") {
    val subgraph = Set(HyperEdge(31, 1, Vector(), EmptyMetadata), HyperEdge(35, 0, Vector(30, 20, 32, 48, 3), EmptyMetadata), HyperEdge(32, 1, Vector(), EmptyMetadata))
    val asHoles: Map[Int, Int] = {
      val creator = Stream.from(0).iterator
      subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
    }

    val pattern: structures.immutable.HyperGraph[Item[Int, Int], Item[Int, Int]] = {
      val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
      structures.immutable.HyperGraph(pEdges.toSeq: _*)
    }

    val tempg = VocabularyHyperGraph(subgraph.toSeq: _*)
    val graph = VersionedHyperGraph(subgraph.toSeq: _*)
    val maps = graph.findSubgraph[Int](pattern)
    val holes = pattern.nodes.collect({ case h: Hole[Int, Int] => h })
    check(holes.forall(h => {
      maps.groupBy(_._1(h.id)).forall(vAndMaps => {
        val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
        updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
      })
    }))
  }

  property("Compaction works for edges with changed sources") {
    val graph = CompactHyperGraph(Seq(HyperEdge(1, 0, Seq(3), EmptyMetadata), HyperEdge(2, 0, Seq(4), EmptyMetadata), HyperEdge(3, 0, Seq.empty, EmptyMetadata)): _*)
    check(graph.+(HyperEdge(4, 0, Seq.empty, EmptyMetadata)).size == 2)
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
    check(forAll { (g: VocabularyHyperGraph[Int, Int]) => {
      g.nodes == g.edges.flatMap(e => e.target +: e.sources)
    }
    })
  }

  property("test FindInNodes works correctly vs naive implementation") {
    check(forAll{ (g: VocabularyHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val s = g.head.edgeType
      g.edges.filter(e => e.target == s || e.sources.contains(s)) == g.findInNodes(s)
    }})
  }
}
