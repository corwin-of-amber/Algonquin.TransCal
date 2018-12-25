package structures.immutable

import org.scalatest.{FlatSpec, Matchers}
import structures.HyperEdge
import structures.HyperGraphManyWithOrderToOneLike.{Hole, Ignored, Item}

class VocabularyHyperGraphSpec extends FlatSpec with Matchers {

  val replacer = HyperEdge[Int, Int](39, 17, Vector(38))
  val replacee = HyperEdge[Int, Int](34, 6, Seq(37, 6, 39, 7))
  val es: Set[HyperEdge[Int, Int]] = Set(replacer, replacee)

  "hypergraph" should "find graph finds edge and replacer" in {
    val g = grapher(es)
    val pg: VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]] =
      grapher(Set(HyperEdge[Item[Int, Int], Item[Int, Int]](Hole(0), Ignored[Int, Int](), replacer.sources.map(_ => Ignored[Int, Int]())),
        HyperEdge[Item[Int, Int], Item[Int, Int]](Ignored(), Ignored[Int, Int](), replacee.sources.map(si => if (si == 39) Hole[Int, Int](0) else Ignored[Int, Int]()))))
    val results = g.findSubgraph[Int, VocabularyHyperGraph[Item[Int, Int], Item[Int, Int]]](pg).map(_.values)
    val foundTarget = results.map(i => i.map {
      case Right(x) => x
      case Left(x) => x
    }).forall(vals => vals.toList.contains(replacer.target))
    results.nonEmpty && foundTarget should be(true)
  }
}

