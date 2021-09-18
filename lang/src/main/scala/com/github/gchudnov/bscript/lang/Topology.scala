package com.github.gchudnov.bscript.lang

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.state.Meta

/**
 * AST + Scopes
 */
final case class Topology(
  ast: AST,
  meta: Meta
)
