package structures

/**
  * @author tomer
  * @since 12/25/18
  */
sealed trait Item[Value, Id]
final case class Explicit[Value, Id](value: Value) extends Item[Value, Id]
final case class Hole[Value, Id](id: Id) extends Item[Value, Id]
final case class Ignored[Value, Id]() extends Item[Value, Id]
