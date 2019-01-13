package synthesis.language

trait Parser[T] {
  def apply(prog: String): T
}
