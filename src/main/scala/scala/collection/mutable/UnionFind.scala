package scala.collection.mutable

import java.util

import scala.collection.mutable

class UnionFind[T](all: collection.Seq[T]) {
  private val dataStruc = {
    val res = new mutable.HashMap[T, mutable.HashSet[T]]
    for (a <- all if (a != null)) res(a) = new mutable.HashSet[T] += a
    res
  }

  /**
    * The number of Unions
    */
  def size: Int = dataStruc.size

  /**
    * Unions the set containing a and b
    */
  def union(a: T, b: T): mutable.HashSet[T] = {
    val first = dataStruc(a)
    val second = dataStruc(b)
    if (first.contains(b)) first
    else {
      //below is to merge smaller with bigger rather than other way around
      val (toMerge, mergeInto) = if (first.size >= second.size) (second, first)
      else (first, second)
      mergeInto ++= toMerge
      toMerge.foreach(a => {
        dataStruc.remove(a)
        dataStruc(a) = mergeInto
      })
      mergeInto
    }
  }

  /**
    * true if they are in same set. false if not
    */
  def find(a: T): collection.Set[T] = {
    dataStruc(a).toSet
  }

  def sets: Predef.Set[Predef.Set[T]] = dataStruc.values.map(s => s.toSet).toSet
}
