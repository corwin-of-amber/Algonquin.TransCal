package structures.mutable

import org.scalacheck.Arbitrary
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import structures.generic.VersionedHyperGraphLikeTest
import synthesis.Programs
import synthesis.actions.ActionSearchState
import synthesis.actions.operators.{DefAction, OperatorRunAction}
import synthesis.rewrites.{RewriteRule, RewriteSearchState}
import transcallang.TranscalParser


class VersionedHyperGraphPropSpec extends VersionedHyperGraphLikeTest[Int, Int, VersionedHyperGraph[Int, Int], VersionedHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VersionedHyperGraph[Int, Int]] = Arbitrary(versionedIntegerGraphGen)
  override implicit def nodeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  override implicit def edgeTypeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VersionedHyperGraph[Int, Int] = VersionedHyperGraph(es.toSeq: _*)
  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VersionedHyperGraph[Item[Int, Int], Item[Int, Int]] = VersionedHyperGraph(es.toSeq: _*)

  property("all constructor") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      new VersionedHyperGraph(new VocabularyHyperGraph(es)).edges == es && VersionedHyperGraph(es.toSeq: _*).edges == es shouldEqual true
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
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
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
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
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
        val version = structures.generic.VersionedHyperGraphLike.VersionMetadata.getEdgeVersion(edge)
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
    val rules = stateWithExistentialRule.rewriteRules
    rules.size shouldEqual 1
    val r = rules.head.asInstanceOf[RewriteRule]
    var curVer = stateWithExistentialRule.programs.hyperGraph.version
    val rewriteState = new RewriteSearchState(stateWithExistentialRule.programs.hyperGraph)
    val (state1, version1) = r(rewriteState, 0)
    val (state2, version2) = r(new RewriteSearchState(CompactHyperGraph(state1.graph.edges.toSeq: _*)), version1)
    val (state3, version3) = r(new RewriteSearchState(CompactHyperGraph(state2.graph.edges.toSeq: _*)), version2)
    version2 shouldEqual version1 + 1
    state3.graph.size shouldEqual 8
  }
}
