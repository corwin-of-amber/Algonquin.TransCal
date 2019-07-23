package synthesis.search

/**
  * @author tomer
  * @since 11/18/18
  */
trait State[+This] {
  def deepCopy(): This
}
