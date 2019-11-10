package synthesis.rewrites

import structures._
import synthesis.rewrites.Template.TemplateTerm
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/16/18
  */
final case class Template(target: TemplateTerm[HyperTermId], function: TemplateTerm[HyperTermIdentifier], parameters: Seq[TemplateTerm[HyperTermId]])

object Template {
  type TemplateTerm[T <: HyperTerm] = Item[T, Int]

  type ExplicitTerm[T <: HyperTerm] = Explicit[T, Int]
  object ExplicitTerm {
    def apply[T <: HyperTerm](t: T): ExplicitTerm[T] = new ExplicitTerm(t)
    def unapply[T <: HyperTerm](arg: ExplicitTerm[T]): Option[T] = Some(arg.value)
  }

  type ReferenceTerm[T <: HyperTerm] = Hole[T, Int]
  object ReferenceTerm{
    def apply[T <: HyperTerm](i: Int): ReferenceTerm[T] = new ReferenceTerm(i)
    def unapply[T <: HyperTerm](arg: ReferenceTerm[T]): Option[Int] = Some(arg.id)
  }

  type IgnoreTerm[T <: HyperTerm] = Ignored[T, Int]
  type RepetitionTerm[T <: HyperTerm] = Repetition[T, Int]

  object RepetitionTerm {
    def rep0[T <: HyperTerm](maxRepetition: Int, repeated: TemplateTerm[T]): RepetitionTerm[T] = Repetition.rep0(maxRepetition, repeated)

    def rep1[T <: HyperTerm](maxRepetition: Int, repeated: TemplateTerm[T]): RepetitionTerm[T] = Repetition.rep1(maxRepetition, repeated)

    def option[T <: HyperTerm](repeated: TemplateTerm[T]): RepetitionTerm[T] = Repetition.option(repeated)

    def rep[T <: HyperTerm](minRepetition: Int, maxRepetition: Int, repeated: TemplateTerm[T]): RepetitionTerm[T] = Repetition.rep(minRepetition, maxRepetition, repeated)

    def rep0[T <: HyperTerm](maxRepetition: Int, repeated: Stream[TemplateTerm[T]]): RepetitionTerm[T] = Repetition.rep0(maxRepetition, repeated)

    def rep1[T <: HyperTerm](maxRepetition: Int, repeated: Stream[TemplateTerm[T]]): RepetitionTerm[T] = Repetition.rep1(maxRepetition, repeated)

    def rep[T <: HyperTerm](minRepetition: Int, maxRepetition: Int, repeated: Stream[TemplateTerm[T]]): RepetitionTerm[T] = Repetition.rep(minRepetition, maxRepetition, repeated)
  }

}
