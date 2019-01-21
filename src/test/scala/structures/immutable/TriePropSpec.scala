package structures.immutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import structures.Explicit

import scala.util.Random


class TriePropSpec extends PropSpec with Checkers {
  implicit val letterCreator = Arbitrary(integerLetterGen)
  implicit val wordCreator = Arbitrary(integerWordGen)
  implicit val trieCreator = Arbitrary(integerTrieGen)

  def checkRemoved(trie: Trie[Int], i: Int): Boolean = {
    val word = trie.words.toList(i)
    !(trie - word).words.contains(word)
  }

  property("all constructor") {
    check(forAll { words: Set[Seq[Int]] =>
      new Trie(words).words == words && Trie(words).words == words
    })
  }

  property("removes") {
    check(forAll { trie: Trie[Int] =>
      trie.words.nonEmpty ==> checkRemoved(trie, Random.nextInt(trie.words.size))
    })
  }

  property("remove non exists") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> ((trie - word) == trie)
    })
  }

  property("add non exists works") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> (trie + word).words.contains(word)
    })
  }

  property("add twice works") {
    check(forAll { word: Seq[Int] =>
      {
        val onceTrie = Trie.empty + word
        val twiceTrie = onceTrie + word
        onceTrie == twiceTrie
      }
    })
  }

  property("empty find prefix returns all") {
    check(forAll { trie: Trie[Int] =>
      val a = trie.findPatternPrefix(Seq())
      trie.words == trie.findPatternPrefix(Seq())
    })
  }

  property("added should be findable as sparse") {
    check(forAll { trie: Trie[Int] =>
      trie.words.forall(word => trie.findRegex(word.map(Explicit(_))).contains(word))
    })
  }

  property("edges finds all that were added twice") {
    check(forAll { word: Seq[Int] =>
      (Trie.empty[Int] + word + word).words.count(_ == word) == 1
    })
  }

  property("edges finds all that were added") {
    check(forAll { words: Set[Seq[Int]] =>
      words.intersect(words.foldLeft(Trie.empty[Int])((trie, word) => trie + word).words).size == words.size
    })
  }

  property("add than remove than add") {
    check(forAll { (trie: Trie[Int], word: Seq[Int]) =>
      !trie.words.contains(word) ==> {
        val trieAdd = trie + word
        trie.words.size + 1 == trieAdd.words.size && {
          val trieRemove = trie - word
          trieAdd.words.size-1 == trieRemove.words.size && {trieRemove.words.size + 1 == (trie + word).words.size}
        }
      }
    })
  }

  property("changed letter to exists letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replace(keepLetter, changeLetter)
      beforeLettersSize == newTrie.letters.size + 1 && trie != newTrie
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
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == newTrie.words.size && trie != newTrie
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
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == trie.words.size && trie != newTrie
    }
    check(forAll { (trie: Trie[Int], keepLetter: Int) =>
      validate(trie, keepLetter, if (trie.letters.isEmpty) 1 else trie.letters.max + 1)
    })
  }
}
