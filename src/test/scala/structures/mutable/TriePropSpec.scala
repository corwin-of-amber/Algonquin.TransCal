package structures.mutable

import org.scalacheck.Arbitrary
import org.scalatest.Assertion
import structures._


class TriePropSpec extends VocabularyTest[Int, Trie[Int]] {
  protected implicit val letterCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  protected implicit val wordCreator: Arbitrary[Seq[Int]] = Arbitrary(integerWordGen)
  protected implicit val trieCreator: Arbitrary[Trie[Int]] = Arbitrary(integerTrieGen)
  override def builder(es: Set[VocabularyLike.Word[Int]]) = Trie(es)

  property("changed exist letter to non exist letter in place") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int): Assertion = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replaceInPlace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored()).get))
      newTrie.letters should have size beforeLettersSize
      newTrie.words should have size beforeWordsSize
      trie shouldBe newTrie
      found shouldBe empty
    }
    val trie = Trie[Int](Set(Vector(5, 10), Vector(6, 21, 3, 9, 10), Vector(9), Vector(20, 19, 8, 1, 8)))
    validate(trie, trie.letters.max + 1, trie.letters.last)
  }
}
