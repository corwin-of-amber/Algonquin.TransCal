package structures.generic

import com.typesafe.scalalogging.LazyLogging
import structures.{Explicit, Hole, Ignored, Repetition, Vocabulary, VocabularyLike}
import structures.VocabularyLike.{Word, WordRegex}

import scala.collection.mutable

trait TrieLike[Letter, +This <: TrieLike[Letter, This]] extends Vocabulary[Letter] with LazyLogging {

  override def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])] = {
    logger.trace("find pattern prefix")
    if (isEmpty) {
      Set.empty
    } else {
      def splitByRepetition(pattern: WordRegex[Letter, Id], index: Int): Set[WordRegex[Letter, Id]] = {
        if (pattern.isEmpty) Set(pattern)
        else pattern.head match {
          case Repetition(minR, maxR, repeated) =>
            val patternMinimalLength = pattern.tail.map({
              case Repetition(m, _, _) => m
              case _ => 1
            }).sum
            assert(repeated.take(scala.math.min(maxR, getSubtries.length - index - patternMinimalLength))
              .forall(!_.isInstanceOf[Repetition[Letter, Id]]))

            val results = (minR to scala.math.min(maxR, getSubtries.length - index - patternMinimalLength)).flatMap(i => {
              val tailRes = splitByRepetition(pattern.tail, index + i)
              val headRes: WordRegex[Letter, Id] = repeated.take(i)
              tailRes.map(headRes ++ _)
            }).toSet
            results
          case x => splitByRepetition(pattern.tail, index+1).map(x +: _)
        }
      }

      splitByRepetition(pattern, 0).flatMap(p => {
        val words = ReFindRegex(p)
        // create map from words
        val holeToIndex = mutable.HashMultiMap.empty[Id, Int]
        p.zipWithIndex.collect({case (Hole(id), i) => (id, i)}).foreach({case (id, i) => holeToIndex.addBinding(id, i)})
        words.map(w => (w, holeToIndex.map({case (id, indexes) => (id, w(indexes.head))}).toMap))
      })
    }
  }

  protected def getSubtries: Seq[collection.Map[Letter, This]]

  protected def getSubtrie(index: Int, value: Letter): Option[This]

  protected def ReFindRegex[Id](pattern: WordRegex[Letter, Id]): Set[Word[Letter]] = {
    // Patterns should be split ahead of time. Maybe later I will do it on demand for now assume not.
    // If we return patterns inside we need to return map in addition to words
    assert(!pattern.exists(_.isInstanceOf[Repetition[Letter, Id]]))

    var curTrie: Option[This] = Some(this.asInstanceOf[This])
    var toSkip = 0
    val holeToIndexes = mutable.HashMultiMap.empty[Id, Int]
    for ((item, i) <- pattern.zipWithIndex) {
      item match {
        case Explicit(value) =>
          curTrie = curTrie.flatMap(t => t.getSubtrie(toSkip, value))
          toSkip = 0
        case Hole(id) =>
          toSkip += 1
          holeToIndexes.addBinding(id, i)
        case Ignored() =>
          toSkip += 1
      }
    }
    // Now I need to filter out words by holes.
    val predicates = holeToIndexes.filter(t => t._2.size > 1).toSeq.map(id_indexes => {
      val indexes = id_indexes._2
      (w: Word[Letter]) => indexes.tail.forall(j => w(indexes.head) == w(j))
    })
    curTrie.map(cTrie => cTrie.words.filter((w: Word[Letter]) => w.length == pattern.length).filter(w => predicates.forall(_ (w)))).getOrElse(Set.empty)
  }

  protected def recursiveFindRegex[Id](pattern: WordRegex[Letter, Id],
                                       placeholdersMap: Map[Id, Letter],
                                       length: Int,
                                       skip: Int): Set[(Word[Letter], Map[Id, Letter])] = {
    def specificValue(value: Letter,
                      more: WordRegex[Letter, Id],
                      placeholdersMap: Map[Id, Letter]): Set[(Word[Letter], Map[Id, Letter])] =
      if (getSubtries.length > skip)
        getSubtries(skip).get(value)
          .map(_.recursiveFindRegex(more, placeholdersMap, length + 1, 0)).getOrElse(Set.empty)
      else Set.empty

    pattern match {
      case Nil => words.filter(_.length == length).zip(Stream.continually(placeholdersMap))
      case item +: more =>
        item match {
          case Explicit(value) =>
            specificValue(value, more, placeholdersMap)
          case Hole(id) =>
            placeholdersMap.get(id)
              .map(specificValue(_, more, placeholdersMap))
              .getOrElse(
                getSubtries.applyOrElse(skip, (_: Int) => Map.empty[Letter, This])
                  .flatMap {
                    case (letter: Letter, subtrie: This) =>
                      subtrie.recursiveFindRegex(more, placeholdersMap updated(id, letter), length + 1, 0)
                  }.toSet
              )
          case Ignored() =>
            recursiveFindRegex(more, placeholdersMap, length + 1, skip + 1)
          case Repetition(minR, maxR, repeated) =>
            assert(repeated.take(scala.math.min(maxR, getSubtries.length - more.length))
              .forall(!_.isInstanceOf[Repetition[Letter, Id]]))
            val results = (
              for (newPattern <- (minR to scala.math.min(maxR, getSubtries.length)).map(i => repeated.take(i) ++ more))
                yield recursiveFindRegex(newPattern, placeholdersMap, length, skip)
              ).flatten.toSet
            results
        }
    }
  }
}
