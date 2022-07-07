package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.b1.internal.*
import com.github.gchudnov.bscript.builder.AstMeta
import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.inspector.Inspector
import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatchDiff
import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatchStashEntry
import com.github.gchudnov.bscript.interpreter.Interpreter
import com.github.gchudnov.bscript.interpreter.InterpreterLaws
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.ast.Block
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.serde.JSONSerde
import com.github.gchudnov.bscript.translator.Translator
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3TypeInit
import com.github.gchudnov.bscript.translator.laws.TypeInit

sealed trait B1:

  val typeNames: TypeNames = B1TypeNames.make()
  val typeInit3: TypeInit = Scala3TypeInit
  val types: Types         = Types.make(typeNames)

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
    val ast1 = if opts.hasPrelude then
      import Block.*
      prelude :+ ast0
    else ast0
    Builder.build(ast1, types, typeCheckLaws)

  /**
   * Interpret AST that was built before
   */
  def interpret(ast1: AST, meta1: Meta): Either[Throwable, Cell] =
    interpret_(ast1, meta1).map(_.retValue)

  def interpret(astMeta1: AstMeta): Either[Throwable, Cell] =
    interpret(astMeta1.ast, astMeta1.meta)

  private def interpret_(ast1: AST, meta1: Meta): Either[Throwable, InterpretState] =
    val interpreterLaws = B1InterpreterLaws.make(types, meta1)
    Interpreter.interpret_(ast1, meta1, interpreterLaws)

  /**
   * Run - Build & Interpret
   */
  def run(ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, Cell] =
    build(ast0, opts).flatMap(interpret)

  /**
   * Debug - Set MemWatch & Build & Interpret
   */
  def debug(path: String, ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, (Cell, Seq[MemWatchDiff])] =
    for
      ast1    <- Inspector.memWatch(CellPath(path), ast0, typeNames)
      is      <- build(ast1, opts).flatMap(am => interpret_(am.ast, am.meta))
      entry    = MemWatchStashEntry.get(is.stash)
      retValue = is.retValue
      log      = entry.log
    yield (retValue, log)

  /**
   * Translate AST to Scala
   */
  def translate(ast0: AST, opts: B1Options = B1Options.default): Either[Throwable, String] =
    build(ast0, opts).flatMap(astMeta => Translator.translateScala3(astMeta.ast, astMeta.meta, typeNames, typeInit3))

object B1 extends B1
