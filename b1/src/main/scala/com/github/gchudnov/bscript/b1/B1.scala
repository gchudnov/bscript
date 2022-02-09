package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.b1.internal.*
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.lang.types.Types

object B1:

  private val typeNames = B1TypeNames.make()
  private val types = Types.make(typeNames)

  private val typeCheckLaws = B1TypeCheckLaws.make(types)

  /**
   * Loads AST from a JSON-string.
   * 
   * NOTE: After loading, the AST must be built before the interpretation.
   */ 
  def load(s: String): Either[Throwable, AST] =
    ???

  /**
   * Save AST to a JSON-string.
   */
  def save(ast: AST): Either[Throwable, String] =
    ???

  /**
   * Build AST
   */
  def build(ast0: AST): Either[Throwable, AST] =
    ???

  /**
   * Interpret AST that was built before
   */
  def interpret(ast1: AST): Either[Throwable, Cell] =
    ???

  /**
   * Translate AST to Scala
   */
  def translate(ast1: AST): Either[Throwable, String] =
    ???
