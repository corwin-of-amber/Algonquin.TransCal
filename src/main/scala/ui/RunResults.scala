package ui

import transcallang.{AnnotatedTree, Datatype}

case class RunResults(knownTypes: Set[Datatype], knownRules: Set[AnnotatedTree], knownRulesDefs: Set[AnnotatedTree], newRules: Set[AnnotatedTree], goal: Option[(AnnotatedTree, AnnotatedTree)], success: Boolean)
