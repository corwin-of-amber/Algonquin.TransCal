package structures.mutable

import scala.collection.AbstractIterator

/**
  * @author tomer
  * @since 11/28/18
  */
class CombineSeq[A](iterators: Seq[Iterator[A]]) extends AbstractIterator[Seq[A]] {
  private val innerIterator: Iterator[Seq[A]] = iterators match {
    case Nil => Iterator.empty
    case head +: Nil => head.map(Seq(_))
    case head +: tail =>  new CombineTwo(head, new CombineSeq(tail)).map(t=>t._1 +: t._2)
  }

  override def hasNext: Boolean = innerIterator.hasNext

  override def next(): Seq[A] = innerIterator.next()
}
