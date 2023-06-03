package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.AST

private[typecheck] final case class PassState(
)

private[typecheck] object PassState:
  
  lazy val empty: PassState =
    PassState(
    )
    
  def from(s: InState): PassState =
    empty

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
    )
