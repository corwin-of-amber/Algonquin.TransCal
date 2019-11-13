package structures.immutable

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.{Explicit, Hole, Ignored, Repetition}

import scala.util.Random


class TriePropSpec extends PropSpec with Matchers with ScalaCheckPropertyChecks {
  private implicit val letterCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  private implicit val wordCreator: Arbitrary[Seq[Int]] = Arbitrary(integerWordGen)
  private implicit val trieCreator: Arbitrary[Trie[Int]] = Arbitrary(integerTrieGen)

  def checkRemoved(trie: Trie[Int], i: Int): Assertion = {
    val word = trie.words.toList(i)
    (trie - word).words should not contain(word)
  }

  property("all constructor") {
    forAll { words: Set[Seq[Int]] =>
      new Trie(words).words shouldEqual words
      Trie(words).words shouldEqual words
    }
  }

  property("removes") {
    forAll(minSize(1)) { trie: Trie[Int] => whenever(trie.nonEmpty) {
      checkRemoved(trie, Random.nextInt(trie.words.size))
    }}
  }

  property("remove non exists") {
    forAll { (trie: Trie[Int], word: Seq[Int]) =>
      whenever(!trie.words.contains(word)) {
        (trie - word) shouldEqual trie
      }
    }
  }

  property("add non exists works") {
    forAll { (trie: Trie[Int], word: Seq[Int]) =>
      whenever(!trie.words.contains(word)) {
        (trie + word).words should contain (word)
      }
    }
  }

  property("add twice works") {
    forAll { word: Seq[Int] =>
        val onceTrie = Trie.empty + word
        val twiceTrie = onceTrie + word
        onceTrie shouldEqual twiceTrie
    }
  }

  property("empty find prefix returns all") {
    forAll { trie: Trie[Int] =>
      trie.words shouldEqual trie.findRegexWords(Seq(Repetition.rep0[Int, Nothing](Int.MaxValue, Ignored()).get))
    }
  }

  property("added should be findable as sparse") {
    forAll { trie: Trie[Int] =>
      trie.words.forall(word => {
        val repetitionInf = Repetition.rep0[Int, Nothing](Int.MaxValue, Ignored()).get
        val regex = word.flatMap(i => Seq(repetitionInf, Explicit(i))) :+ repetitionInf
        trie.findRegexWords(regex).contains(word)
      }) shouldBe true
    }
  }

  property("added should be findable") {
    forAll { trie: Trie[Int] =>
      trie.words.forall(word => trie.findRegexWords(word.map(Explicit(_))).contains(word)) shouldBe true
    }
  }

  property("edges finds all that were added twice") {
    forAll { word: Seq[Int] =>
      (Trie.empty[Int] + word + word).words.count(_ == word) shouldBe 1
    }
  }

  property("edges finds all that were added") {
    forAll { words: Set[Seq[Int]] =>
      words.intersect(words.foldLeft(Trie.empty[Int])(_ + _).words) should have size words.size
    }
  }

  property("Should find correct explicit in big graph") {
    forAll(minSize(100)) { trie: Trie[Int] =>
      whenever(trie.size > 100 && trie.words.exists(_.length > 3)) {
        trie.words.filter(_.length > 3).forall(w =>
          trie.findRegexWords(Seq(Hole(0), Hole(1), Hole(2), Explicit(w(3)), Repetition.rep0[Int, Int](Int.MaxValue, Ignored()).get))
            .forall(_(3) == w(3))) shouldBe true
      }
    }
  }

  property("add than remove than add") {
    forAll { (trie: Trie[Int], word: Seq[Int]) =>
      whenever(!trie.words.contains(word)) {
        val trieAdd = trie + word
        trie.words should have size (trieAdd.words.size -1)
        val trieRemove = trie - word
        trieAdd.words should have size (trieRemove.words.size + 1)
        trieRemove.words should have size ((trie + word).words.size - 1)
      }
    }
  }

  property("changed letter to exists letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int): Assertion = {
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()).get, Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored()).get))
      newTrie.letters should have size (beforeLettersSize - 1)
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
      found shouldBe (empty)
    }
    forAll(minSize(1)) { trie: Trie[Int] => whenever(trie.nonEmpty) {
      validate(trie, trie.letters.max + 1, trie.letters.last)
    }}
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
