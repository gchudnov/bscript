package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeAsts

private[builder] final case class InState(
  ast: AST,
  scopeAsts: ScopeAsts,
)

private[builder] object InState:
  def from(ast: AST, scopeAsts: ScopeAsts): InState =
    InState(
      ast = ast,
      scopeAsts = scopeAsts,
    )
