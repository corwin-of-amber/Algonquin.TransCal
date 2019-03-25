package synthesis

import transcallang.TranscalParser

/**
  * @author tomer
  * @since 3/25/19
  */
object ProgramsDB {
  private val parser = new TranscalParser()
  private val terms = Seq(
    "type true boolean = true",
    "type false boolean = true",
    "type 0 integer = true",
    "type 1 integer = true",
    "type ⟨⟩ list<`a> = true"
  ).map(parser.apply)
  val programs = terms.foldLeft(Programs.empty)(_ addTerm _)
}
