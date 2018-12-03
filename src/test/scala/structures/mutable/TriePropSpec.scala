package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

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

  property("edges finds all that were added") {
    check(forAll { words: Seq[Seq[Int]] =>
      val trie = new Trie[Int]()
      for (word <- words) trie.add(word)
      words.toSet.intersect(trie.words).size == words.size
    })
  }

  property("add than remove than add") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> (trie.words.size + 1 == trie.add(word).words.size &&
        trie.words.size-1 == trie.remove(word).words.size && trie.words.size + 1 == trie.add(word).words.size)
    })
  }

  property("changed exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
//      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size + 1 /*&& beforeWordsSize == trie.words.size*/ && trie == newTrie
    }
    check(forAll { (trie: Trie[Int], keepLetter: Int, changeLetter: Int) =>
      trie.letters.contains(changeLetter) ==> validate(trie, keepLetter, changeLetter)
    })
  }

  property("changed non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == trie.words.size && trie == newTrie
    }
    check(forAll { (trie: Trie[Int], keepLetter: Int, changeLetter: Int) =>
      !trie.letters.contains(changeLetter) ==> validate(trie, keepLetter, changeLetter)
    })
  }
}
