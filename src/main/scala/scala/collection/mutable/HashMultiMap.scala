package scala.collection
package mutable

/**
  * @author tomer
  * @since 12/25/18
  */
object HashMultiMap {

  def empty[Key, Value]: mutable.MultiMap[Key, Value] = new mutable.HashMap[Key, mutable.Set[Value]] with mutable.MultiMap[Key, Value]

  def apply[Key, Value](k2vs: collection.Map[Key, collection.Set[Value]]): mutable.MultiMap[Key, Value] = {
    empty[Key, Value] ++ k2vs
  }

  def put[Key, Value](multimap: mutable.MultiMap[Key, Value], kvs: (Key, collection.Set[Value])): mutable.MultiMap[Key, Value] = kvs._2.foldLeft(multimap)((multimap, value) => {multimap. addBinding(kvs._1, value)})
}
