package structures.mutable

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{DefAction, LetAction, OperatorRunAction}
import synthesis.rewrites.RewriteRule
import transcallang.TranscalParser

import scala.util.Random


class VersionedHyperGraphPropSpec extends PropSpec with Matchers with ScalaCheckPropertyChecks with HyperGraphLikeTest[Int, Int, VersionedHyperGraph[Int, Int], VersionedHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VersionedHyperGraph[Int, Int]] = Arbitrary(versionedIntegerGraphGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VersionedHyperGraph[Int, Int] = VersionedHyperGraph(es.toSeq: _*)
  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VersionedHyperGraph[Item[Int, Int], Item[Int, Int]] = VersionedHyperGraph(es.toSeq: _*)

  property("all constructor") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      new VersionedHyperGraph(new VocabularyHyperGraph(es)).edges == es && VersionedHyperGraph(es.toSeq: _*).edges == es shouldEqual true
    }
  }

  property("find graph finds nothing") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      val g = grapher(es)
      val edges = Seq(HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Explicit(800), Seq(Ignored(), Ignored()), EmptyMetadata),
        HyperEdge(Explicit(800), Ignored(), Seq(), EmptyMetadata),
        HyperEdge(Ignored(), Ignored(), Seq(Explicit(800)), EmptyMetadata)).map(Set(_))
      edges.foreach(e => {
        val pg = patterner(e)
        g.findSubgraph[Int](pg) shouldBe empty})
    }
  }

  property("find regex rep0 with 0 sources") {
    val graph = grapher(Set(HyperEdge(0, 1, Seq.empty, EmptyMetadata)))
    val pattern = HyperEdge(Explicit(0), Explicit(1), List(Repetition.rep0(500, Ignored()).get), EmptyMetadata)
    val found = graph.findRegexHyperEdges(pattern)
    val edges = graph.edges
    found shouldEqual edges
  }

  property("If graphs are equal then hash is equal") {
    // Not necessarily true because of compaction
    forAll { es: Set[HyperEdge[Int, Int]] =>
      VersionedHyperGraph(es.toSeq: _*).hashCode shouldEqual VersionedHyperGraph(es.toSeq: _*).hashCode()
    }
  }

  property("Specific - Find Versioned subgraph with and without merge returns same maps") {
    val es = Set(HyperEdge(40, 88, Vector(), EmptyMetadata), HyperEdge(14, 88, Vector(39, 48, 13, 46, 7), EmptyMetadata), HyperEdge(4, 12, Vector(17, 11, 29, 10, 33), EmptyMetadata), HyperEdge(14, 88, Vector(), EmptyMetadata))
    val graph = VersionedHyperGraph(es.toSeq: _*)

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
    asHoles.forall(vh => {
      maps.groupBy(_._1(vh._2)).forall(vAndMaps => {
        val updatedMaps = vAndMaps._2.map(mm => (mm._1.filter(_._1 != vh._2), mm._2))
        updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))
      })
    }) shouldEqual true
  }

  property("test edges are found in their versions") {
    forAll{ g: VersionedHyperGraph[Int, Int] => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph =  structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned[Int](hyperGraph, version - 1).nonEmpty
      } shouldEqual true
    }}
  }

  property("test edges are found in versions before them") {
    forAll{ g: VersionedHyperGraph[Int, Int] => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.forall{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph =  structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version).nonEmpty
      } shouldEqual true
    }}
  }

  property("test edges are not found in versions after them") {
    forAll{ g: VersionedHyperGraph[Int, Int] => whenever(g.nonEmpty) {
      val edges = g.edges
      edges.foreach{edge =>
        val version = structures.generic.VersionedHyperGraph.VersionMetadata.getEdgeVersion(edge)
        val regexEdge: HyperEdgePattern[Int, Int, Int] = HyperEdge(Explicit(edge.target), Explicit(edge.edgeType), edge.sources.map(Explicit(_)), EmptyMetadata)
        val hyperGraph = structures.immutable.HyperGraph(regexEdge)
        g.findSubgraphVersioned(hyperGraph, version + 1) shouldBe empty
      }
    }}
  }

  property("test versions using existential rule to verify only new edges are ran and all of them are ran") {
    val parser = new TranscalParser
    val stateWithExistentialRule = new DefAction(parser apply "f ?x >> f ?y")(ActionSearchState(Programs.empty, Set.empty))
    val newState = new OperatorRunAction(maxSearchDepth = 3)(stateWithExistentialRule)
    newState.programs.hyperGraph.size shouldEqual 8
  }

  property("test versions using existential rule while rebuilding graph") {
    val parser = new TranscalParser
    val stateWithExistentialRule = new DefAction(parser apply "f ?x >> f ?y")(ActionSearchState(Programs.empty, Set.empty))
    val opAction = new OperatorRunAction(maxSearchDepth = 1)
    val state1 = opAction(stateWithExistentialRule)
    val state2 = opAction(ActionSearchState(Programs(CompactHyperGraph(state1.programs.hyperGraph.edges.toSeq: _*)), state1.rewriteRules))
    val state3 = opAction(ActionSearchState(Programs(CompactHyperGraph(state2.programs.hyperGraph.edges.toSeq: _*)), state2.rewriteRules))
    state3.programs.hyperGraph.size shouldEqual 8
  }
}
