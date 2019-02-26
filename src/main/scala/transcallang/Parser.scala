package transcallang

trait Parser[T] {
  def apply(prog: String): T
}
