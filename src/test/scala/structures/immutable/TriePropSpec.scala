package structures.immutable

import org.scalacheck.Arbitrary
import org.scalatest.Assertion
import structures._


class TriePropSpec extends VocabularyTest[Int, Trie[Int]] {
  protected implicit val letterCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  protected implicit val wordCreator: Arbitrary[Seq[Int]] = Arbitrary(integerWordGen)
  protected implicit val trieCreator: Arbitrary[Trie[Int]] = Arbitrary(integerTrieGen)
  override def builder(es: Set[VocabularyLike.Word[Int]]) = Trie(es)

  property("changed letter to exists letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int): Assertion = {
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored()).get))
      newTrie.letters should have size beforeLettersSize - 1
      trie should not equal newTrie
      found shouldBe empty
    }
    forAll { trie: Trie[Int] =>
      whenever(trie.letters.size > 1) {
        validate(trie, trie.letters.head, trie.letters.last)
      }
    }
  }

  property("changed exist letter to non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int): Assertion = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored()).get))
      newTrie.letters should have size beforeLettersSize
      newTrie.words should have size beforeWordsSize
      trie should not equal newTrie
      found shouldBe empty
    }
    forAll(minSize(1)) { trie: Trie[Int] =>
      whenever(trie.letters.nonEmpty) {
        validate(trie, trie.letters.max + 1, trie.letters.last)
      }
    }
  }

  property("changed non exist letter keeps the trie the same") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int): Assertion = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored()).get))
      newTrie.letters should have size beforeLettersSize
      trie.words should have size beforeWordsSize
      found shouldBe empty
    }
    forAll { (trie: Trie[Int], keepLetter: Int) =>
      validate(trie, keepLetter, if (trie.letters.isEmpty) 1 else trie.letters.max + 1)
    }
  }

  property("Should find same explicit words in all graphs") {
    forAll(minSize(1)) { trie: Trie[Int] => whenever(trie.nonEmpty) {
      val byLength = trie.words.groupBy(_.length)
      for ((key, words) <- byLength) {
        val regex = 0 until key map Hole[Int, Int]
        trie.findRegexWords(regex) shouldEqual words
        for (i <- 0 until key) {
          val byValue = words.groupBy(_(i))
          for((v, vWords) <- byValue) {
            trie.findRegexWords(regex.updated(i, Explicit(v))) shouldEqual vWords
          }
        }
      }
    }}
  }
}
