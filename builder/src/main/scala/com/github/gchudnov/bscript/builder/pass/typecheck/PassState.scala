package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.AST

final case class PassState(
)

object PassState:
  
  lazy val empty: PassState =
    PassState(
    )
    
  def from(s: InState): PassState =
    empty

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
    )
