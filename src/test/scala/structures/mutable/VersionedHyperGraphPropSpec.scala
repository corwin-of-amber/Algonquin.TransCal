package structures.mutable

import org.scalacheck.Arbitrary
import structures.{EmptyMetadata, Explicit, Hole, HyperEdge, Item}
import structures.HyperGraphLike.HyperEdgePattern
import structures.generic.VersionedHyperGraphLikeTest
import synthesis.Programs
import synthesis.search.ActionSearchState
import synthesis.search.actions.{DefAction, OperatorRunAction}
import synthesis.search.rewrites.RewriteRule
import transcallang.TranscalParser


class VersionedHyperGraphPropSpec extends VersionedHyperGraphLikeTest[Int, Int, VersionedHyperGraph[Int, Int], VersionedHyperGraph[Item[Int, Int], Item[Int, Int]]] {
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

  property("cloned inner versioned grpahs") {
    forAll(minSize(2)) { es: Set[HyperEdge[Int, Int]] =>
      whenever(es.size > 1) {
        val g = VersionedHyperGraph.empty[Int, Int]
        var lastEdge: HyperEdge[Int, Int] = null
        for (e <- es) {
          lastEdge = e
          g += e
        }
        val pattern: HyperGraph[Item[Int, Int], Item[Int, Int]] = HyperGraph(HyperEdge(Explicit(lastEdge.target), Explicit(lastEdge.edgeType), lastEdge.sources.map(s => Explicit(s)), EmptyMetadata))
        g.findSubgraph[Int](pattern) shouldEqual g.findSubgraphVersioned[Int](pattern)
        val newG = g.clone
        newG.mergeNodesInPlace(10000, lastEdge.target)
        g.findSubgraph[Int](pattern) shouldEqual g.findSubgraphVersioned[Int](pattern)
      }
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
    val pattern: structures.generic.HyperGraph[Item[Int, Int], Item[Int, Int]] = {
      val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
      structures.generic.HyperGraph(pEdges.toSeq: _*)
    }

    val maps = graph.findSubgraph[Int](pattern)
    asHoles.forall(vh => {
      maps.groupBy(_.nodeMap(vh._2)).forall(vAndMaps => {
        val updatedMaps = vAndMaps._2.map(mm =>  (mm.copy(nodeMap = mm.nodeMap.filter(_._1 != vh._2))))
        updatedMaps subsetOf graph.findSubgraph[Int](pattern.mergeNodes(Explicit[Int, Int](vAndMaps._1), Hole[Int, Int](vh._2)))
      })
    }) shouldEqual true
  }

  property("test versions using existential rule to verify only new edges are ran and all of them are ran") {
    val parser = new TranscalParser
    val stateWithExistentialRule = new DefAction(parser apply "f ?x >> f ?y")(new ActionSearchState(Programs.empty, Set.empty[RewriteRule]))
    val newState = new OperatorRunAction(maxSearchDepth = 3)(stateWithExistentialRule)
    newState.programs.queryGraph.size shouldEqual 8
  }

  // TODO: Fix this - will only be true with copybuilder for compact graph
  //  property("test versions using existential rule while copying graph") {
  //    val parser = new TranscalParser
  //    val stateWithExistentialRule = new DefAction(parser apply "f ?x >> f ?y")(ActionSearchState(Programs.empty, Set.empty))
  //    val rules = stateWithExistentialRule.rewriteRules
  //    rules.size shouldEqual 1
  //    val r = rules.head.asInstanceOf[RewriteRule]
  //    var curVer = stateWithExistentialRule.programs.hyperGraph.version
  //    val rewriteState = new RewriteSearchState(stateWithExistentialRule.programs.hyperGraph)
  //    val (state1, version1) = r(rewriteState, 0)
  //    val (state2, version2) = r(new RewriteSearchState(CompactHyperGraph(state1.graph.edges.toSeq: _*)), version1)
  //    val (state3, version3) = r(new RewriteSearchState(CompactHyperGraph(state2.graph.edges.toSeq: _*)), version2)
  //    version2 shouldEqual version1 + 1
  //    state3.graph.size shouldEqual 8
  //  }
}
