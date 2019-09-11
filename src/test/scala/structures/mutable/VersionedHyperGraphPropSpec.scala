package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.Checkers
import structures.HyperGraphLike.HyperEdgePattern
import structures._

import scala.util.Random


class VersionedHyperGraphPropSpec extends PropSpec with Checkers with Matchers with HyperGraphLikeTest[Int, Int, VersionedHyperGraph[Int, Int], VersionedHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VersionedHyperGraph[Int, Int]] = Arbitrary(versionedIntegerGraphGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VersionedHyperGraph[Int, Int] = VersionedHyperGraph(es.toSeq: _*)
  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VersionedHyperGraph[Item[Int, Int], Item[Int, Int]] = VersionedHyperGraph(es.toSeq: _*)

  property("all constructor") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      new VersionedHyperGraph(new VocabularyHyperGraph(es)).edges == es && VersionedHyperGraph(es.toSeq: _*).edges == es
    })
  }

  property("find graph finds nothing") {
    check(forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = new VersionedHyperGraph(grapher(es))
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
    val graph = new VersionedHyperGraph(grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata))))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
    val found = graph.findRegexHyperEdges(pattern)
    val edges = graph.edges
    check(found == edges)
  }

  property("If graphs are equal then hash is equal") {
    // Not necessarily true because of compaction
    check(forAll { es: Set[HyperEdge[Int, Int]] => VersionedHyperGraph(es.toSeq: _*).hashCode == VersionedHyperGraph(es.toSeq: _*).hashCode() })
  }

  property("Specific - Find Versioned subgraph with and without merge returns same maps") {
    val es = Set(HyperEdge(40, 88, Vector(), EmptyMetadata), HyperEdge(14, 88, Vector(39, 48, 13, 46, 7), EmptyMetadata), HyperEdge(4, 12, Vector(17, 11, 29, 10, 33), EmptyMetadata), HyperEdge(14, 88, Vector(), EmptyMetadata))
    val graph = VersionedHyperGraph(es.toSeq: _*)
    val temp = VocabularyHyperGraph(es.toSeq: _*)
    val temp1 = CompactHyperGraph(es.toSeq: _*)

    val subgraph = Set(HyperEdge(40, 88, Vector(39, 48, 13, 46, 7), EmptyMetadata), HyperEdge(40, 88, Vector(), EmptyMetadata))
    val asHoles: Map[Int, Int] = {
      val creator = Stream.from(0).iterator
      subgraph.flatMap(e => e.target +: e.sources).map((_, creator.next())).toMap
    }
    val pattern:  structures.immutable.HyperGraph[Item[Int, Int], Item[Int, Int]] = {
      val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
      structures.immutable.HyperGraph(pEdges.toSeq: _*)
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

  property("test edges are found in their versions") {
    check(forAll{ (g: VersionedHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph =  structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned[Int](hyperGraph, version - 1).nonEmpty
      }
    }})
  }

  property("test edges are found in versions before them") {
    check(forAll{ (g: VersionedHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph =  structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version).nonEmpty
      }
    }})
  }

  property("test edges are not found in versions after them") {
    check(forAll{ (g: VersionedHyperGraph[Int, Int]) => g.nonEmpty ==> {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version + 1).isEmpty
      }
    }})
  }
}
