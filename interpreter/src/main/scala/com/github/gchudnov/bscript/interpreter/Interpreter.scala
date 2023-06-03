package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.builder.BuildState
import com.github.gchudnov.bscript.interpreter.pass.interpret.PassImpl
import com.github.gchudnov.bscript.interpreter.pass.interpret.InState

import scala.util.control.Exception.*

/**
 *
 * High-Level Interpreter
 *
 * Because symbol table management happens at run-time in an interpreter, it’s easy to confuse resolving a variable with loading its value. Just keep in mind that resolving a
 * variable means figuring out which program entity it refers to. We can do this without even running the program for statically typed languages. Loading a variable, on the other
 * hand, is purely a run-time operation.
 *
 * We resolve a variable to figure out the space in which its value lives.
 *
 * The parser deals with symbol scopes, and the interpreter deals with memory spaces. Memory spaces don’t do double duty as scopes.
 *
 * During execution, though, we still need scope information to resolve symbols.
 */
sealed trait Interpreter:

  /**
   * Interprets AST.
   *
   * NOTE: AST must be built *before* the interpretation.
   */
  def interpret(ast1: AST, state: BuildState): Either[Throwable, (Cell, InterpretState)] =
    val interpretPass = new PassImpl()

    val in = InState.from(ast1)

    for {
      outState <- nonFatalCatch.either(interpretPass.run(in))
    } yield outState

    ???



  // /**
  //  * Interprets AST.
  //  *
  //  * Low-level API. Returns an interpreter state.
  //  */
  // def interpret_(ast1: AST, meta: Meta, laws: InterpreterLaws): Either[Throwable, InterpretState] =
  //   val globalMemoryName = "globals"
  //   val ms               = MemorySpace(globalMemoryName)
  //   val stash            = Stash.empty

  //   val interpretVisitor = InterpretVisitor.make(laws)
  //   val interpretState   = InterpretState.make(meta = meta, stash = stash, memSpace = ms, retValue = VoidCell)

  //   ast1
  //     .visit(interpretState, interpretVisitor)

object Interpreter extends Interpreter
