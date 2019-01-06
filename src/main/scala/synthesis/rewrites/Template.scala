package synthesis.rewrites

import synthesis.rewrites.Template.TemplateTerm
import synthesis.{HyperTerm, HyperTermId, HyperTermIdentifier}

/**
  * @author tomer
  * @since 11/16/18
  */
final case class Template(target: TemplateTerm[HyperTermId], function: TemplateTerm[HyperTermIdentifier], parameters: Seq[TemplateTerm[HyperTermId]])

object Template {
  sealed trait TemplateTerm[T <: HyperTerm]
  case class ExplicitTerm[T <: HyperTerm](term: T) extends TemplateTerm[T]
  case class ReferenceTerm[T <: HyperTerm](id: Int) extends TemplateTerm[T]
}
