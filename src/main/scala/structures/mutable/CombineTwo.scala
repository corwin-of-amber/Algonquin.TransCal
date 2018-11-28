package structures.mutable


import scala.collection.AbstractIterator

/**
  * @author tomer
  * @since 11/28/18
  */
class CombineTwo[A, B](iter1: Iterator[A], given: Iterator[B]) extends AbstractIterator[(A, B)] {

  private var (kept, iter2) = given.duplicate
  private var chached: Option[A] = if (iter1.hasNext) { Some(iter1.next) } else { None }

  override def hasNext: Boolean = (iter1.hasNext && kept.hasNext) || // can reload
    (chached.isDefined && iter2.hasNext)  // reload is readable

  override def next(): (A, B) = {
    if (!iter2.hasNext) { // Reload
      chached = Some(iter1.next())
      (kept, iter2) = kept.duplicate
    }
    (chached.get, iter2.next())
  }
}
