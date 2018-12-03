package structures

import org.scalacheck.Gen
import org.scalacheck.Gen._
import structures.HyperGraphManyWithOrderToOneLike.HyperEdge

import scala.util.Random

package object mutable {
  def HyperEdgeGenFactory[Node, Edge](nodeSource: Gen[Node], edgeSource: Gen[Edge]): Gen[HyperEdge[Node, Edge]] = for {
    source <- nodeSource
    edge <- edgeSource
    sourcesSize <- Random.nextInt(6)
    sources <- containerOfN[Seq, Node](sourcesSize, nodeSource)
  } yield HyperEdge(source, edge, sources)

  val integerEdgesGen: Gen[HyperEdge[Int, Int]] = HyperEdgeGenFactory(oneOf(0 to 50), oneOf(0 to 20))

  def grapher[Node, Edge](se: Set[HyperEdge[Node, Edge]]): VocabularyHyperGraph[Node, Edge] = {
    val v = new VocabularyHyperGraph[Node, Edge]()
    for (e <- se) v.addEdge(e)
    v
  }

  def HyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[VocabularyHyperGraph[Node, Edge]] = {
    containerOf[Set, HyperEdge[Node, Edge]](edgeSource) map grapher
  }

  val integerGraphGen: Gen[VocabularyHyperGraph[Int, Int]] = HyperGraphGenFactory(integerEdgesGen)

  def WordGenFactory[Letter](letterSource: Gen[Letter]): Gen[Seq[Letter]] = choose( 1, 7).flatMap(containerOfN[Seq, Letter](_, letterSource))

  val integerLetterGen: Gen[Int] = oneOf(0 to 20)
  val integerWordGen: Gen[Seq[Int]] = WordGenFactory(integerLetterGen)

  def TrieGenFactory[Letter](wordSource: Gen[Seq[Letter]]): Gen[Trie[Letter]] = {
    def builder(se: Seq[Seq[Letter]]): Trie[Letter] = {
      val v = new Trie[Letter]()
      for (e <- se) v.add(e)
      v
    }

    containerOf[Seq, Seq[Letter]](wordSource) map builder
  }

  val integerTrieGen: Gen[Trie[Int]] = TrieGenFactory(integerWordGen)
}
