package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.lang.ast.AST

private[interpreter] final case class OutState(
  ast: AST
)

private[interpreter] object OutState {}