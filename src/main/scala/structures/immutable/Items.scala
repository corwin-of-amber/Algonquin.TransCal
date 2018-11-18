package structures.immutable

/**
  * @author tomer
  * @since 11/16/18
  */

class Item[Value, Id]
case class Reference[Value, Id](id: Id) extends Item[Value, Id]
case class Explicit[Value, Id](value: Value) extends Item[Value, Id]
case class NotMatter[Value, Id]() extends Item[Value, Id]