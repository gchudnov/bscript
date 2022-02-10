package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.b1.internal.*
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.Block
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.serde.JSONSerde
import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.builder.AstMeta
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.interpreter.Interpreter
import com.github.gchudnov.bscript.interpreter.InterpreterLaws
import com.github.gchudnov.bscript.translator.Translator

sealed trait B1:

  val typeNames = B1TypeNames.make()
  val types     = Types.make(typeNames)

  private val typeCheckLaws = B1TypeCheckLaws.make(types)

  private lazy val serde = JSONSerde.make()

  private lazy val prelude = B1Prelude.make(typeNames)

  /**
   * Loads AST from a JSON-string.
   *
   * NOTE: After loading, the AST must be built before the interpretation.
   */
  def load(s: String): Either[Throwable, AST] =
    serde.deserialize(s)

  /**
   * Save AST to a JSON-string.
   */
  def save(ast: AST): Either[Throwable, String] =
    serde.serialize(ast)

  /**
   * Build AST
   */
  def build(ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, AstMeta] =
    val ast = if opts.hasPrelude then
      import Block.*
      prelude :+ ast0
    else ast0
    Builder.build(ast, types, typeCheckLaws)

  /**
   * Interpret AST that was built before
   */
  def interpret(ast1: AST, meta1: Meta): Either[Throwable, Cell] =
    val interpreterLaws = B1InterpreterLaws.make(types, meta1)
    Interpreter.interpret(ast1, meta1, interpreterLaws)

  def interpret(astMeta1: AstMeta): Either[Throwable, Cell] =
    interpret(astMeta1.ast, astMeta1.meta)

  /**
   * Run - Build & Interpret
   */
  def run(ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, Cell] =
    build(ast0, opts).flatMap(interpret)

  /**
   * Translate AST to Scala
   */
  def translate(ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, String] =
    build(ast0, opts).flatMap(astMeta => Translator.translateScala(astMeta.ast, astMeta.meta, typeNames))

object B1 extends B1
