package structures

import structures.immutable.Item

/**
  * @author tomer
  * @since 11/15/18
  */
trait VocabularyLike[Letter, +This <: VocabularyLike[Letter, This]] {

  def findByPrefix(tuple: Seq[(Int, Letter)]): Set[Seq[Letter]]

  def find(tuple: Seq[(Int, Letter)], size: Int): Set[Seq[Letter]] = findByPrefix(tuple).filter(_.length == size)

  def findPatternPrefix[Id](pattern: Seq[Item[Letter, Id]]): Set[Seq[Letter]]

  def findPattern[Id](pattern: Seq[Item[Letter, Id]]): Set[Seq[Letter]] = findPatternPrefix(pattern).filter(_.length == pattern.length)

  def isEmpty: Boolean = words.isEmpty

  def words: Set[Seq[Letter]]

  def letter: Set[Letter] = words.flatten

  def replace(keep: Letter, change: Letter): This

  def remove(word: Seq[Letter]): This

  def add(word: Seq[Letter]): This
}
