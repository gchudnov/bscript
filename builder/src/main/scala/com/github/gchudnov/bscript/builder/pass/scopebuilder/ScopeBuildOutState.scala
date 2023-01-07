package com.github.gchudnov.bscript.builder.pass.scopebuilder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts

final case class ScopeBuildOutState(
  ast: AST,
  forest: Forest[Scope],
  scopeSymbols: ScopeSymbols, 
  scopeAsts: ScopeAsts
)
