package structures.generic

import com.typesafe.scalalogging.LazyLogging
import structures.{Explicit, Hole, Ignored, Repetition, Vocabulary, VocabularyLike}
import structures.VocabularyLike.{Word, WordRegex}

trait TrieLike[Letter, +This <: TrieLike[Letter, This]] extends Vocabulary[Letter] with LazyLogging {

  override def findRegex[Id](pattern: WordRegex[Letter, Id]): Set[(Word[Letter], Map[Id, Letter])] = {
    logger.trace("find pattern prefix")
    if (isEmpty) {
      Set.empty
    } else {
      recursiveFindRegex[Id](pattern, Map.empty, 0, 0)
    }
  }

  protected def getSubtries: Seq[collection.Map[Letter, This]]

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
