package structures

/**
  * @author tomer
  * @since 12/25/18
  */
sealed trait Item[+Value, +Id]
object Item {
  def itemsValueToMap[T, Id](tuples: Seq[(Item[T, Id], T)]): Map[Id, T] = {
    tuples.toMap.collect{ case (k: Hole[T, Id], v) => (k.id , v)}
  }
}
final case class Explicit[+Value, +Id](value: Value) extends Item[Value, Id]
final case class Hole[+Value, +Id](id: Id) extends Item[Value, Id]
final case class Ignored[+Value, +Id]() extends Item[Value, Id]
final class Repetition[+Value, +Id] private (val minRepetition: Int, val maxRepetition: Int, val repeated: Stream[Item[Value, Id]]) extends Item[Value, Id] {
  require(0 <= minRepetition)
  require(minRepetition <= maxRepetition)
  require(0 < maxRepetition)
  override def toString: String = f"Repetition($minRepetition, $maxRepetition, Stream)"
}

object Repetition {
  def rep0[Value, Id](maxRepetition: Int, repeated: Item[Value, Id]): Repetition[Value, Id] = rep(0, maxRepetition, repeated)
  def rep1[Value, Id](maxRepetition: Int, repeated: Item[Value, Id]): Repetition[Value, Id] = rep(1, maxRepetition, repeated)
  def option[Value, Id](repeated: Item[Value, Id]): Repetition[Value, Id] = rep(0, 1, repeated)
  def rep[Value, Id](minRepetition: Int, maxRepetition: Int, repeated: Item[Value, Id]): Repetition[Value, Id] = {
    rep(minRepetition, maxRepetition, Stream.continually(repeated))
  }

  def unapply[Value, Id](arg: Repetition[Value, Id]): Option[(Int, Int, Stream[Item[Value, Id]])] = Some(arg.minRepetition, arg.maxRepetition, arg.repeated)
  def rep0[Value, Id](maxRepetition: Int, repeated: Stream[Item[Value, Id]]): Repetition[Value, Id] = rep(0, maxRepetition, repeated)
  def rep1[Value, Id](maxRepetition: Int, repeated: Stream[Item[Value, Id]]): Repetition[Value, Id] = rep(1, maxRepetition, repeated)
  def rep[Value, Id](minRepetition: Int, maxRepetition: Int, repeated: Stream[Item[Value, Id]]): Repetition[Value, Id] = {
    new Repetition(minRepetition, maxRepetition, repeated)
  }
}
