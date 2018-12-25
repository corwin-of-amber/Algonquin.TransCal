package structures

/**
  * @author tomer
  * @since 12/25/18
  */
case class Explicit[Value, Id](value: Value) extends Item[Value, Id]
