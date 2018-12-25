package structures

import scala.collection.mutable

/**
  * @author tomer
  * @since 12/25/18
  */
object MultiMap {

  def empty[Key, Value]: mutable.MultiMap[Key, Value] = new mutable.HashMap[Key, mutable.Set[Value]] with mutable.MultiMap[Key, Value]

  def apply[Key, Value](k2vs: Map[Key, Set[Value]]): mutable.MultiMap[Key, Value] = k2vs.foldLeft(empty[Key, Value])(put)

  def put[Key, Value](multimap: mutable.MultiMap[Key, Value], kvs: (Key, Set[Value])): mutable.MultiMap[Key, Value] = kvs._2.foldLeft(multimap)((multimap, value) => {multimap. addBinding(kvs._1, value)})
}
