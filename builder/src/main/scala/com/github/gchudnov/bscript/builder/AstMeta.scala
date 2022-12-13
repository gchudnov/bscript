package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Meta

final case class AstMeta(
  ast: AST,
  meta: Meta
)
