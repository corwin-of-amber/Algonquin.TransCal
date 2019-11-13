package structures

import org.scalacheck.Arbitrary
import org.scalatest.{Assertion, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import structures.VocabularyLike.Word

import scala.util.Random

trait VocabularyTest[
  Letter,
  V <: VocabularyLike[Letter, V] with scala.collection.Set[structures.VocabularyLike.Word[Letter]]
] extends PropSpec with Matchers with ScalaCheckPropertyChecks {

  protected implicit val letterCreator: Arbitrary[Letter]
  protected implicit val wordCreator: Arbitrary[Word[Letter]]
  protected implicit val trieCreator: Arbitrary[V]
  def builder(es: Set[Word[Letter]]): V

  def checkRemoved(trie: V, i: Int): Assertion = {
    val word = trie.words.toList(i)
    (trie - word).words should not contain word
  }

  property("all constructor") {
    forAll { words: Set[Word[Letter]] =>
      builder(words).words shouldEqual words
    }
  }

  property("removes") {
    forAll { trie: V =>
      whenever(trie.words.nonEmpty) {
        checkRemoved(trie, Random.nextInt(trie.words.size))
      }
    }
  }

  property("remove non exists") {
    forAll { (trie: V, word: Word[Letter]) =>
      whenever(!trie.words.contains(word)) {
        (trie - word) shouldEqual trie
      }
    }
  }

  property("add non exists works") {
    forAll { (trie: V, word: Word[Letter]) =>
      whenever(!trie.words.contains(word)) {
        (trie + word).words should contain (word)
      }
    }
  }

  property("add twice works") {
    forAll { word: Word[Letter] =>
      val onceTrie = builder(Set.empty) + word
      val twiceTrie = onceTrie + word
      onceTrie shouldEqual twiceTrie
    }
  }

  property("empty find prefix returns all") {
    forAll { trie: V =>
      trie.words shouldEqual trie.findRegexWords(Seq(Repetition.rep0[Letter, Int](Int.MaxValue, Ignored()).get))
    }
  }

  property("added should be findable as sparse") {
    forAll { trie: V =>
      for (word <- trie.words) {
        val repetitionInf = Repetition.rep0[Letter, Int](Int.MaxValue, Ignored()).get
        val regex = word.flatMap(i => Seq(repetitionInf, Explicit(i))) :+ repetitionInf
        trie.findRegexWords(regex) should contain (word)
      }
    }
  }

  property("added should be findable") {
    forAll { trie: V =>
      for(word <- trie.words) {
        trie.findRegexWords(word.map(Explicit(_))) should contain (word)
      }
    }
  }

  property("edges finds all that were added twice") {
    forAll { word: Word[Letter] =>
      (builder(Set.empty) + word + word).words.count(_ == word) shouldBe 1
    }
  }

  property("edges finds all that were added") {
    forAll { words: Set[Word[Letter]] =>
//      words.intersect(words.foldLeft(builder(Set.empty))(_ + _)) should have size words.size
    }
  }

  property("Should find correct explicit in big graph") {
    forAll { trie: V =>
      whenever(trie.size > 100 && trie.words.exists(_.length > 3)) {
        trie.words.filter(_.length > 3).forall(w =>
          trie.findRegexWords(Seq(Hole(0), Hole(1), Hole(2), Explicit(w(3)), Repetition.rep0[Letter, Int](Int.MaxValue, Ignored()).get))
            .forall(_(3) == w(3))) shouldBe true
      }
    }
  }

  property("add than remove than add") {
    forAll { (trie: V, word: Word[Letter]) =>
      whenever(!trie.words.contains(word)) {
        val trieAdd = trie + word
        trieAdd.words should have size trie.words.size + 1
        val trieRemove = trie - word
        trieRemove.words should have size trieAdd.words.size-1
        (trie + word).words should have size trieRemove.words.size + 1
      }
    }
  }
}
