package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.internal.InterpretVisitor
import com.github.gchudnov.bscript.interpreter.internal.InterpretVisitor.InterpretState
import com.github.gchudnov.bscript.builder.state.Meta

sealed trait Interpreter:

  /**
   * Interprets AST.
   *
   * NOTE: AST must be built before the interpretation.
   */
  def interpret(ast1: AST, meta: Meta, laws: InterpretLaws): Either[Throwable, Cell] =
    val globalMemoryName = "globals"
    val ms               = MemorySpace(globalMemoryName)

    val interpretVisitor = InterpretVisitor.make(laws)
    val interpretState   = InterpretState.make(meta = meta, memSpace = ms, retValue = VoidCell)

    ast1
      .visit(interpretState, interpretVisitor)
      .map(_.retValue)

object Interpreter extends Interpreter
