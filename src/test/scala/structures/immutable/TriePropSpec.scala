package structures.immutable

import org.scalacheck.Arbitrary
import structures._


class TriePropSpec extends VocabularyTest[Int, Trie[Int]] {
  protected implicit val letterCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  protected implicit val wordCreator: Arbitrary[Seq[Int]] = Arbitrary(integerWordGen)
  protected implicit val trieCreator: Arbitrary[Trie[Int]] = Arbitrary(integerTrieGen)
  override def builder(es: Set[VocabularyLike.Word[Int]]) = Trie(es)
}
