package structures.immutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.immutable.Trie

import scala.util.Random


class TriePropSpec extends PropSpec with Checkers {
  implicit val letterCreator = Arbitrary(integerLetterGen)
  implicit val wordCreator = Arbitrary(integerWordGen)
  implicit val trieCreator = Arbitrary(integerTrieGen)

  def checkRemoved(trie: Trie[Int], i: Int): Boolean = {
    val words = trie.words.toList(i)
    !trie.remove(words).words.contains(words)
  }

  property("removes") {
    check(forAll { trie: Trie[Int] =>
      trie.words.nonEmpty ==> checkRemoved(trie, Random.nextInt(trie.words.size))
    })
  }

  property("remove non exists") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> (trie.remove(word) == trie)
    })
  }

  property("add non exists works") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> trie.add(word).words.contains(word)
    })
  }

  property("edges finds all that were added twice") {
    check(forAll { word: Seq[Int] =>
      val trie = new Trie[Int]()
      trie.add(word)
      trie.add(word)
      trie.words.count(_ == word) == 1
    })
  }

  property("edges finds all that were added") {
    check(forAll { words: Set[Seq[Int]] =>
      val trie = new Trie[Int]()
      for (word <- words) trie.add(word)
      words.intersect(trie.words).size == words.size
    })
  }

  property("add than remove than add") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> (trie.words.size + 1 == trie.add(word).words.size &&
        trie.words.size-1 == trie.remove(word).words.size && trie.words.size + 1 == trie.add(word).words.size)
    })
  }

  property("changed letter to exists letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size + 1 && trie == newTrie
    }
    check(forAll { trie: Trie[Int] =>
      (trie.letters.size > 1) ==> validate(trie, trie.letters.head, trie.letters.last)
    })
  }

  property("changed exist letter to non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == newTrie.words.size && trie == newTrie
    }
    check(forAll { trie: Trie[Int] =>
      trie.letters.nonEmpty ==> validate(trie, trie.letters.max + 1, trie.letters.last)
    })
  }

  property("changed non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == trie.words.size && trie == newTrie
    }
    check(forAll { (trie: Trie[Int], keepLetter: Int) =>
      validate(trie, keepLetter, if (trie.letters.isEmpty) 1 else trie.letters.max + 1)
    })
  }
}
