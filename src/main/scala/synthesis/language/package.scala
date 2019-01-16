package synthesis

import syntax.Identifier

package object language {
  val builtinConsts: Seq[String] = Seq("⟨⟩", "true", "false", "⊤", "⊥")
  val builtinNotOps: Seq[String] = Seq("~", "¬")
  val builtinSetArithOps: Seq[String] = Seq("++", "+", "-", "∪")
  val builtinSetBuildingOps: Seq[String] = Seq(":+", "::")
  val builtinBooleanOps: Seq[String] = Seq("<->", "\\/", "∨", "/\\", "∧", "=", "≠", "!=", "∈", "∉", ", , ", "‖", "<", ">", "<=", ">=", "≤", "≥")
  val builtinDragOps: Seq[String] = Seq(":", "/", "↦", "->", "=>")
  
  val builtinCommands: Seq[String] = Seq("->", "→", "<-", "←", "[]", "□")

  val arity: Map[String, Int] =
    (builtinBooleanOps ++ builtinSetArithOps ++ builtinSetArithOps).zip(Stream continually 2).toMap ++
      builtinNotOps.zip(Stream continually 1).toMap ++
      builtinConsts.zip(Stream continually 0).toMap
}
