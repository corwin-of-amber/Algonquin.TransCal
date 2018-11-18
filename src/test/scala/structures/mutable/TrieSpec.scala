package structures.mutable

import org.scalatest.{FlatSpec, Matchers}

import java.security.SecureRandom
import structures.Vocabulary

class TrieSpec extends FlatSpec with Matchers {

  val random = new SecureRandom()

  "Trie" should "add a word" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val word = randomWord

    vocabulary add word

    vocabulary.words should have size 1
    vocabulary.words should contain (word)
  }

  it should "add a word once" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val word = randomWord

    vocabulary add word
    vocabulary add word

    vocabulary.words should have size 1
    vocabulary.words should contain (word)
  }

  it should "add different words" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val word1 = randomWord
    val word2 = randomWord

    vocabulary add word1
    vocabulary add word2

    vocabulary.words should have size 2
    vocabulary.words should contain (word1)
    vocabulary.words should contain (word2)
  }

  it should "remove only the word" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val word = randomWord

    vocabulary add word
    vocabulary remove word

    vocabulary.words shouldBe empty
  }

  it should "remove only the removed word" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val word1 = randomWord
    val word2 = word1 ++ randomWord

    vocabulary add word1
    vocabulary add word2
    vocabulary remove word1

    vocabulary.words should have size 1
    vocabulary.words should contain (word2)
  }

  it should "change the letter" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val word = Seq(change)
    val expected = Seq(keep)

    vocabulary add word
    vocabulary replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "change only the letter" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val otherSuffix = randomWord
    val word = change +: otherSuffix
    val expected = keep +: otherSuffix

    vocabulary add word
    vocabulary replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "change only the letter and merge new words" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val otherSuffix = randomWord
    val word = change +: otherSuffix
    val expected = keep +: otherSuffix

    vocabulary add word
    vocabulary add expected
    vocabulary replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "return all when empty find prefix" in {
    val vocabulary: Vocabulary[Int] = Trie.empty[Int]

    for (i <- 0 to random.nextInt(MAX_WORD_SIZE)) {
      vocabulary.add(randomWord)
    }

    vocabulary.findByPrefix(Seq.empty) shouldBe vocabulary.words
  }

  /* --- Privates --- */

  private val MAX_WORD_SIZE = 5
  private val MIN_WORD_SIZE = 1
  private def randomWord: Seq[Int] = {
    (0 to (random.nextInt(MAX_WORD_SIZE - MIN_WORD_SIZE) + MIN_WORD_SIZE)) map (_ => random.nextInt)
  }
}
