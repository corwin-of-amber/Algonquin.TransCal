package structures.immutable

import org.scalatest.{FlatSpec, Matchers}
import java.security.SecureRandom


class TrieSpec extends FlatSpec with Matchers {

  val random = new SecureRandom()

  "Trie" should "add a word" in {
    val first = Trie.empty[Int]
    val word = randomWord

    val vocabulary = first add word

    vocabulary.words should have size 1
    vocabulary.words should contain (word)
  }

  it should "add a word once" in {
    val first = Trie.empty[Int]
    val word = randomWord

    val vocabulary = first add word add word

    vocabulary.words should have size 1
    vocabulary.words should contain (word)
  }

  it should "add different words" in {
    val first = Trie.empty[Int]
    val word1 = randomWord
    val word2 = randomWord

    val vocabulary = first add word1 add word2

    vocabulary.words should have size 2
    vocabulary.words should contain (word1)
    vocabulary.words should contain (word2)
  }

  it should "remove only the word" in {
    val first = Trie.empty[Int]
    val word = randomWord

    val vocabulary = first add word remove word

    vocabulary.words shouldBe empty
  }

  it should "remove only the removed word" in {
    val first = Trie.empty[Int]
    val word1 = randomWord
    val word2 = word1 ++ randomWord

    val vocabulary = first add word1 add word2 remove word1

    vocabulary.words should have size 1
    vocabulary.words should contain (word2)
  }

  it should "change the letter" in {
    val first = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val word = Seq(change)
    val expected = Seq(keep)

    val vocabulary = first add word replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "change only the letter" in {
    val first = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val otherSuffix = randomWord
    val word = change +: otherSuffix
    val expected = keep +: otherSuffix

    val vocabulary = first add word replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "change only the letter and merge new words" in {
    val first = Trie.empty[Int]
    val keep = random.nextInt
    val change = random.nextInt
    val otherSuffix = randomWord
    val word = change +: otherSuffix
    val expected = keep +: otherSuffix

    val vocabulary = first add word add expected replace (keep, change)

    vocabulary.words should have size 1
    vocabulary.words should contain (expected)
  }

  it should "return all when empty find prefix" in {
    var vocabulary = Trie.empty[Int]

    for (i <- 0 to random.nextInt(MAX_WORD_SIZE)) {
      vocabulary = vocabulary.add(randomWord)
    }

    vocabulary.findPatternPrefix(Seq.empty) shouldBe vocabulary.words
  }

  /* --- Privates --- */

  private val MAX_WORD_SIZE = 5
  private val MIN_WORD_SIZE = 1
  private def randomWord: Seq[Int] = {
    (0 to (random.nextInt(MAX_WORD_SIZE - MIN_WORD_SIZE) + MIN_WORD_SIZE)) map (_ => random.nextInt)
  }
}
