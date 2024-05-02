package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.asm.laws.AsmTranslateLaws
import com.github.gchudnov.bscript.translator.into.scala3.Scala3Import
import com.github.gchudnov.bscript.translator.laws.TypeInit
import com.github.gchudnov.bscript.translator.{TranslateLaws, Translator}

/**
 * Translates AST to Scala
 */
private[translator] final class AsmTranslator(laws: TranslateLaws, state: AsmState) extends Translator:
  override def fromAST(ast1: AST): Either[Throwable, String] =
    val cVisitor = AsmVisitor.make(laws)

    ast1
      .visit(state, cVisitor)
      .map(ss => ss.show())

  override inline def toAST[T](inline x: T): AST =
    Scala3Import.make(x)

object AsmTranslator {
  def make(meta: Meta, typeNames: TypeNames): Translator =
    val typeInit = AsmTypeInit
    val typeNa = AsmTypeNA
    val laws     = AsmTranslateLaws.make(typeNames, typeInit, typeNa, meta)
    val state    = AsmState.make(meta)
    new AsmTranslator(laws, state)

}