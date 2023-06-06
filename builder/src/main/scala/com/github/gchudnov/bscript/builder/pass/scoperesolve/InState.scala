package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols

private[builder] final case class InState(
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts,
  ast: AST
)

private[builder] object InState:
  def from(scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts, ast: AST): InState =
    InState(
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
      ast = ast,
    )
