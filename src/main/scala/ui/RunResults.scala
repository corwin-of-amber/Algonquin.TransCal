package ui

import transcallang.{AnnotatedTree, Datatype}

case class RunResults(knownTypes: Set[Datatype], knownRules: Set[AnnotatedTree], knownRulesDefs: Set[AnnotatedTree], newRules: Set[AnnotatedTree], goals: Set[(AnnotatedTree, AnnotatedTree)], successGoals: Set[(AnnotatedTree, AnnotatedTree)])
