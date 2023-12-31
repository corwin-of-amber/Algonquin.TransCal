package structures.mutable

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.Checkers
import structures.{Explicit, Hole, Ignored, Repetition}

import scala.util.Random


class TriePropSpec extends PropSpec with Checkers {
  private implicit val letterCreator: Arbitrary[Int] = Arbitrary(integerLetterGen)
  private implicit val wordCreator: Arbitrary[Seq[Int]] = Arbitrary(integerWordGen)
  private implicit val trieCreator: Arbitrary[Trie[Int]] = Arbitrary(integerTrieGen)

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
      trie.words == trie.findRegexWords(Seq(Repetition.rep0[Int, Nothing](Int.MaxValue, Ignored())))
    })
  }

  property("added should be findable as sparse") {
    check(forAll { trie: Trie[Int] =>
      trie.words.forall(word => {
        val repetitionInf = Repetition.rep0[Int, Nothing](Int.MaxValue, Ignored())
        val regex = word.flatMap(i => Seq(repetitionInf, Explicit(i))) :+ repetitionInf
        trie.findRegexWords(regex).contains(word)
      })
    })
  }

  property("added should be findable") {
    check(forAll { trie: Trie[Int] =>
      trie.words.forall(word => trie.findRegexWords(word.map(Explicit(_))).contains(word))
    })
  }

  property("edges finds all that were added twice") {
    check(forAll { word: Seq[Int] =>
      (Trie.empty[Int] + word + word).words.count(_ == word) == 1
    })
  }

  property("edges finds all that were added") {
    check(forAll { words: Set[Seq[Int]] =>
      words.intersect(words.foldLeft(Trie.empty[Int])(_ + _).words).size == words.size
    })
  }

  property("Should find correct explicit in big graph") {
    check(forAll { trie: Trie[Int] => (trie.size > 100 && trie.words.exists(_.length > 3)) ==> {
      trie.words.filter(_.length > 3).forall(w =>
        trie.findRegexWords(Seq(Hole(0), Hole(1), Hole(2), Explicit(w(3)), Repetition.rep0[Int, Int](Int.MaxValue, Ignored())))
          .forall(_(3) == w(3)))
    }})
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
      val newTrie = trie.replaceNotInPlace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()), Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored())))
      beforeLettersSize == newTrie.letters.size + 1 && trie != newTrie && found.isEmpty
    }
    check(forAll { trie: Trie[Int] =>
      (trie.letters.size > 1) ==> validate(trie, trie.letters.head, trie.letters.last)
    })
  }

  property("changed exist letter to non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replaceNotInPlace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()), Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored())))
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == newTrie.words.size && trie != newTrie && found.isEmpty
    }
    check(forAll { trie: Trie[Int] =>
      trie.letters.nonEmpty ==> validate(trie, trie.letters.max + 1, trie.letters.last)
    })
  }

  property("Specific - changed exist letter to non exist letter") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replaceNotInPlace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()), Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored())))
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == newTrie.words.size && trie != newTrie && found.isEmpty
    }
    val trie = Trie[Int](Set(Vector(5, 10), Vector(6, 21, 3, 9, 10), Vector(9), Vector(20, 19, 8, 1, 8)))
    check(validate(trie, trie.letters.max + 1, trie.letters.last))
  }

  property("changed non exist letter keeps the trie the same") {
    def validate(trie: Trie[Int], keepLetter: Int, changeLetter: Int) = {
      val beforeWordsSize = trie.words.size
      val beforeLettersSize = trie.letters.size
      val newTrie = trie.replaceNotInPlace(keepLetter, changeLetter)
      val found = newTrie.findRegex(Seq(Repetition.rep0(Int.MaxValue, Ignored()), Explicit(changeLetter), Repetition.rep0(Int.MaxValue, Ignored())))
      beforeLettersSize == newTrie.letters.size && beforeWordsSize == trie.words.size && found.isEmpty
    }
    check(forAll { (trie: Trie[Int], keepLetter: Int) =>
      validate(trie, keepLetter, if (trie.letters.isEmpty) 1 else trie.letters.max + 1)
    })
  }

  property("Should find same explicit words in all graphs") {
    check(forAll { trie: Trie[Int] => trie.nonEmpty ==> {
      val byLength = trie.words.groupBy(_.length)
      byLength.forall(kv => {
        val key = kv._1
        val words = kv._2
        val regex = 0 until key map Hole[Int, Int]
        val canFindAll = trie.findRegexWords(regex) == words
        val allValsFound = for (i <- 0 until key) yield {
          val byValue = words.groupBy(_(i))
          val canFindAllWithExplicit = for((v, vWords) <- byValue) yield {
            trie.findRegexWords(regex.updated(i, Explicit(v))) == vWords
          }
          canFindAllWithExplicit.forall(_ == true)
        }
        canFindAll && allValsFound.forall(_ == true)
      })
    }})
  }
}
