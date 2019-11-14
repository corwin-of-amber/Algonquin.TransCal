package structures.immutable

import org.scalacheck.Arbitrary
import structures.HyperGraphLike.HyperEdgePattern
import structures._
import structures.generic.VersionedHyperGraphLikeTest


class VersionedHyperGraphPropSpec extends VersionedHyperGraphLikeTest[Int, Int, VersionedHyperGraph[Int, Int], VersionedHyperGraph[Item[Int, Int], Item[Int, Int]]]{
  implicit def edgeCreator: Arbitrary[HyperEdge[Int, Int]] = Arbitrary(integerEdgesGen)
  implicit def graphCreator: Arbitrary[VersionedHyperGraph[Int, Int]] = Arbitrary(versionedIntegerGraphGen)
  override implicit def nodeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  override implicit def edgeTypeCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)

  override def grapher(es: Set[HyperEdge[Int, Int]]): VersionedHyperGraph[Int, Int] = VersionedHyperGraph(es.toSeq: _*)
  override def patterner(es: Set[HyperEdgePattern[Int, Int, Int]]): VersionedHyperGraph[Item[Int, Int], Item[Int, Int]] = VersionedHyperGraph(es.toSeq: _*)

  property("all constructor") {
    forAll { es: Set[HyperEdge[Int, Int]] =>
      new VersionedHyperGraph(new VocabularyHyperGraph(es)).edges shouldEqual es
      VersionedHyperGraph(es.toSeq: _*).edges shouldEqual es
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
    val pattern: HyperGraph[Item[Int, Int], Item[Int, Int]] = {
      val pEdges = subgraph.map(e => e.copy(Hole(asHoles(e.target)), Explicit(e.edgeType), e.sources.map(x => Hole(asHoles(x)))))
      HyperGraph(pEdges.toSeq: _*)
    }

    val maps = graph.findSubgraph[Int](pattern)
    asHoles.forall(vh => {
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
    }) shouldEqual true
  }
}
