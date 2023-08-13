package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.util.Tree

private[builder] final case class InState(
  ast: AST,
  scopeTree: Tree[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts,
)

private[builder] object InState:
  def from(ast: AST, scopeTree: Tree[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): InState =
    InState(
      ast = ast,
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
    )
