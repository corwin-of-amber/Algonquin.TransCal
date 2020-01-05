package structures.mutable

import org.scalacheck.Arbitrary
import structures.{EmptyMetadata, Explicit, Hole, HyperEdge, Item}
import structures.HyperGraphLike.HyperEdgePattern
import synthesis.rewrites.Template.ExplicitTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language, TranscalParser}

import scala.util.Random


class CompactHyperGraphPropSpec extends HyperGraphLikeTest[Int, Int, CompactHyperGraph[Int, Int], CompactHyperGraph[Item[Int, Int], Item[Int, Int]]] {
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[CompactHyperGraph[Int, Int]] = Arbitrary(compactIntegerGraphGen)
  override implicit def nodeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  override implicit def edgeTypeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): CompactHyperGraph[Int, Int] = CompactHyperGraph(es.toSeq: _*)

  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): CompactHyperGraph[Item[Int, Int], Item[Int, Int]] = CompactHyperGraph(es.toSeq: _*)

  property("Compactions works correctly on mutliple swithces of same var") {
    val graphs = Programs.destructPatterns(new TranscalParser().apply("(?x ≤ ?y) ||> min(x, y) >> id x").subtrees)
    graphs(1).edges.head.sources.head shouldEqual graphs.head.edges.find(_.edgeType.asInstanceOf[ExplicitTerm[HyperTermIdentifier]].value.identifier.literal == "≤").get.sources.head
    graphs(1).edges.head.sources.head shouldEqual graphs.head.edges.find(_.edgeType.asInstanceOf[ExplicitTerm[HyperTermIdentifier]].value.identifier.literal == "min").get.sources.head
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

  property("Find Compact subgraph with and without merge returns same maps") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      whenever(es.nonEmpty) {
        val subgraph = Random.shuffle(es).take(Random.nextInt(es.size))
        val asHoles: Map[Int, Int] = {
          val creator = Stream.from(0).iterator
          subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
        }
        val pattern: HyperGraph[Item[Int, Int], Item[Int, Int]] = {
          val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
          HyperGraph(pEdges.toSeq: _*)
        }
        val graph = CompactHyperGraph(es.toSeq: _*)
        val maps = graph.findSubgraph[Int](pattern)
        val holes = pattern.nodes.filter(_.isInstanceOf[Hole[Int, Int]]).map(_.asInstanceOf[Hole[Int, Int]])
        holes.forall(h => {
          maps.groupBy(_._1(h.id)).forall(vAndMaps => {
            val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != h.id), mm._2))
            updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), h))
          })
        }) shouldEqual true
      }
    }
  }

  property("Specific - Find Versioned subgraph with and without merge returns same maps") {
    val es = Set(HyperEdge(40, 88, Vector(), EmptyMetadata), HyperEdge(14, 88, Vector(39, 48, 13, 46, 7), EmptyMetadata), HyperEdge(4, 12, Vector(17, 11, 29, 10, 33), EmptyMetadata), HyperEdge(14, 88, Vector(), EmptyMetadata))
    val graph = CompactHyperGraph(es.toSeq: _*)

    val subgraph = Set(HyperEdge(40, 88, Vector(39, 48, 13, 46, 7), EmptyMetadata), HyperEdge(40, 88, Vector(), EmptyMetadata))
    val asHoles: Map[Int, Int] = {
      val creator = Stream.from(0).iterator
      subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
    }
    val pattern: HyperGraph[Item[Int, Int], Item[Int, Int]] = {
      val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
      HyperGraph(pEdges.toSeq: _*)
    }

    val maps = graph.findSubgraph[Int](pattern)
    for (vh <- asHoles) {
      for (vAndMaps <- maps.groupBy(_._1(vh._2))) {
        val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != vh._2), mm._2))
        if (!updatedMaps.subsetOf(graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2))))) {
          println(s"working on $vh with $vAndMaps")
          println(s"pattern is $pattern")
          println(s"merged is ${pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2))}")
          println(s"new maps: $updatedMaps")
          println(s"subgraph results ${graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))}")
        }
        (updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))) shouldBe true
      }
    }
  }

  property("Compaction works for edges with changed sources") {
    val graph = CompactHyperGraph(Seq(HyperEdge(1, 0, Seq(3), EmptyMetadata), HyperEdge(2, 0, Seq(4), EmptyMetadata), HyperEdge(3, 0, Seq.empty, EmptyMetadata)): _*)
    graph.+(HyperEdge(4, 0, Seq.empty, EmptyMetadata)) should have size 2
  }

  property("Serialization doesn't change graph") {
    implicit val graphCreator: Arbitrary[CompactHyperGraph[HyperTermId, HyperTermIdentifier]] = Arbitrary(compactHyperGraphGen)
    forAll { es: CompactHyperGraph[HyperTermId, HyperTermIdentifier] =>
      es shouldEqual CompactHyperGraph.fromJson(CompactHyperGraph.toJson(es))
    }
  }
}
