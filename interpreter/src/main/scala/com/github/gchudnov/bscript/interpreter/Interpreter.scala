package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.internal.InterpretVisitor
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.internal.Stash
import com.github.gchudnov.bscript.builder.state.Meta

sealed trait Interpreter:

  /**
   * Interprets AST.
   *
   * NOTE: AST must be built before the interpretation.
   */
  def interpret(ast1: AST, meta: Meta, laws: InterpreterLaws): Either[Throwable, Cell] =
    interpret_(ast1, meta, laws)
      .map(_.retValue)

  /**
   * Interprets AST.
   *
   * Low-level API. Returns an interpreter state.
   */
  def interpret_(ast1: AST, meta: Meta, laws: InterpreterLaws): Either[Throwable, InterpretState] =
    val globalMemoryName = "globals"
    val ms               = MemorySpace(globalMemoryName)
    val stash            = Stash.empty

    val interpretVisitor = InterpretVisitor.make(laws)
    val interpretState   = InterpretState.make(meta = meta, stash = stash, memSpace = ms, retValue = VoidCell)

    ast1
      .visit(interpretState, interpretVisitor)

object Interpreter extends Interpreter
