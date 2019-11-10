package mymath

object PartialOrderingOps {
  def max[T](seq: Seq[T])(implicit partialOrdering: PartialOrdering[T]): Seq[T] = {
    var maximums = scala.collection.mutable.Seq.empty[T]
    for(elem <- seq) {
      val newMaximums = maximums.filter(partialOrdering.gt(_, elem))
      maximums = newMaximums
      if (!maximums.exists(partialOrdering.gt(_, elem))) {
        maximums = elem +: maximums
      }
    }
    maximums
  }
}
