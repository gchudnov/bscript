package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeAsts

private[builder] final case class OutState(
  ast: AST,
  scopeAsts: ScopeAsts,
)
