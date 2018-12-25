package structures

import org.scalacheck.Gen
import org.scalacheck.Gen._

import scala.util.Random

package object immutable {
  def HyperEdgeGenFactory[Node, Edge](nodeSource: Gen[Node], edgeSource: Gen[Edge]): Gen[HyperEdge[Node, Edge]] = for {
    source <- nodeSource
    edge <- edgeSource
    sourcesSize <- Random.nextInt(6)
    sources <- containerOfN[Seq, Node](sourcesSize, nodeSource)
  } yield HyperEdge(source, edge, sources)

  val integerEdgesGen: Gen[HyperEdge[Int, Int]] = HyperEdgeGenFactory(oneOf(0 to 50), oneOf(0 to 20))

  def grapher[Node, Edge](se: Set[HyperEdge[Node, Edge]]): VocabularyHyperGraph[Node, Edge] = VocabularyHyperGraph(se)

  def HyperGraphGenFactory[Node, Edge](edgeSource: Gen[HyperEdge[Node, Edge]]): Gen[VocabularyHyperGraph[Node, Edge]] = {
    containerOf[Set, HyperEdge[Node, Edge]](edgeSource) map grapher
  }

  val integerGraphGen: Gen[VocabularyHyperGraph[Int, Int]] = HyperGraphGenFactory(integerEdgesGen)

  def WordGenFactory[Letter](letterSource: Gen[Letter]): Gen[Seq[Letter]] = choose( 1, 7).flatMap(containerOfN[Seq, Letter](_, letterSource))

  val integerLetterGen: Gen[Int] = oneOf(0 to 20)
  val integerWordGen: Gen[Seq[Int]] = WordGenFactory(integerLetterGen)

  def TrieGenFactory[Letter](wordSource: Gen[Seq[Letter]]): Gen[Trie[Letter]] = {
    containerOf[Set, Seq[Letter]](wordSource) map Trie[Letter]
  }

  val integerTrieGen: Gen[Trie[Int]] = TrieGenFactory(integerWordGen)
}
