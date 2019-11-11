package scala.collection.mutable

import scala.collection.generic.Shrinkable

trait ShrinkableBuilder[-Elem, +To] extends collection.mutable.Builder[Elem, To] with Shrinkable[Elem]