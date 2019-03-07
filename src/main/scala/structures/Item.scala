package structures

/**
  * @author tomer
  * @since 12/25/18
  */
sealed trait Item[+Value, +Id]
object Item {
  def itemsValueToMap[T, Id](tuples: Seq[(Item[T, Id], T)]): Map[Id, T] = {
    tuples.filter(t => t._1.isInstanceOf[Hole[T, Id]])
      .map(t => (t._1.asInstanceOf[Hole[T, Id]].id, t._2)).toMap
  }
}
final case class Explicit[+Value, +Id](value: Value) extends Item[Value, Id]
final case class Hole[+Value, +Id](id: Id) extends Item[Value, Id]
final case class Ignored[+Value, +Id]() extends Item[Value, Id]
final case class Repetition[+Value, +Id] private (minRepetition: Int, maxRepetition: Int, repeated: Item[Value, Id]) extends Item[Value, Id]
object Repetition {
  def rep0[Value, Id](maxRepetition: Int, repeated: Item[Value, Id]): Option[Repetition[Value, Id]] = rep(0, maxRepetition, repeated)
  def rep1[Value, Id](maxRepetition: Int, repeated: Item[Value, Id]): Option[Repetition[Value, Id]] = rep(1, maxRepetition, repeated)
  def option[Value, Id](repeated: Item[Value, Id]): Option[Repetition[Value, Id]] = rep(0, 1, repeated)
  def rep[Value, Id](minRepetition: Int, maxRepetition: Int, repeated: Item[Value, Id]): Option[Repetition[Value, Id]] = {
    if (0 <= minRepetition && minRepetition < maxRepetition) Some({
      repeated match {
        case Repetition(minR, maxR, rep1) => new Repetition(minR + minRepetition, maxR + maxRepetition, repeated)
        case _ => new Repetition(minRepetition, maxRepetition, repeated)
      }
    })
    else None
  }
}
