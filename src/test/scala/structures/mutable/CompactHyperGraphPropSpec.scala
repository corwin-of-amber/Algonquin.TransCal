package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import synthesis.rewrites.Template.ExplicitTerm
import synthesis.{HyperTermId, HyperTermIdentifier, Programs}
import transcallang.{Identifier, Language, TranscalParser}

import scala.util.Random


class CompactHyperGraphPropSpec extends PropSpec with Checkers with Matchers  with HyperGraphLikeTest[Int, Int, CompactHyperGraph[Int, Int], CompactHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[CompactHyperGraph[Int, Int]] = Arbitrary(compactIntegerGraphGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): CompactHyperGraph[Int, Int] = CompactHyperGraph(es.toSeq: _*)
  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): CompactHyperGraph[Item[Int, Int], Item[Int, Int]] = CompactHyperGraph(es.toSeq: _*)


  property("find graph finds nothing") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(800), Seq(Ignored(), Ignored()), EmptyMetadata),
        HyperEdge(Explicit(800), Ignored(), Seq(), EmptyMetadata),
        HyperEdge(Ignored(), Ignored(), Seq(Explicit(800)), EmptyMetadata)).map(Set(_))
      edges.forall(e => {
        val pg = patterner(e)
        g.findSubgraph[Int](pg).isEmpty
      }
      )
    })
  }

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = new HyperGraphLike.HyperEdgePattern[Int, Int, Int](Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored())), EmptyMetadata)
    val found = graph.findRegexHyperEdges[Int](pattern)
    val edges = graph.edges
    check(found == edges)
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

  property("Find Compact subgraph with and without merge returns same maps") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      es.nonEmpty ==> {
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
        })
      }
    })
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
    check(asHoles.forall(vh => {
      maps.groupBy(_._1(vh._2)).forall(vAndMaps => {
        val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != vh._2), mm._2))
        if (!updatedMaps.subsetOf(graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2))))) {
          println(s"working on $vh with $vAndMaps")
          println(s"pattern is $pattern")
          println(s"merged is ${pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2))}")
          println(s"new maps: $updatedMaps")
          println(s"subgraph results ${graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))}")
        }
        updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))
      })
    }))
  }

  property("Compaction works for edges with changed sources") {
    val graph = CompactHyperGraph(Seq(HyperEdge(1, 0, Seq(3), EmptyMetadata), HyperEdge(2, 0, Seq(4), EmptyMetadata), HyperEdge(3, 0, Seq.empty, EmptyMetadata)): _*)
    check(graph.+(HyperEdge(4, 0, Seq.empty, EmptyMetadata)).size == 2)
  }

  property("test edges are found in their versions") {
    check(forAll{ g: CompactHyperGraph[Int, Int] => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned[Int](hyperGraph, version - 1).nonEmpty
      }
    }})
  }

  property("test edges are found in versions before them") {
    check(forAll{ g: CompactHyperGraph[Int, Int] => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version).nonEmpty
      }
    }})
  }

  property("test edges are not found in versions after them") {
    check(forAll{ g: CompactHyperGraph[Int, Int] => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version + 1).isEmpty
      }
    }})
  }
}
