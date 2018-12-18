package synthesis.rewrites

import synthesis.HyperTerm

/**
  * @author tomer
  * @since 11/16/18
  */
case class Template(target: Template.TemplateTerm, function: Template.TemplateTerm, parameters: Seq[Template.TemplateTerm])

object Template {
  trait TemplateTerm
  case class ExplicitTerm(term: HyperTerm) extends TemplateTerm {
    def unapply(arg: ExplicitTerm): HyperTerm = arg.term
  }
  case class ReferenceTerm(id: Int) extends TemplateTerm {
    def unapply(arg: ReferenceTerm): Int = arg.id
  }
}
