package synthesis.Language

trait Parser[T] {
  def apply(prog: String): T
}
