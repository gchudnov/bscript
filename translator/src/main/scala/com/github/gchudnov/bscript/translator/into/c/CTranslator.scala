package com.github.gchudnov.bscript.translator.into.c

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.{TranslateLaws, Translator}
import com.github.gchudnov.bscript.translator.into.scala3.Scala3Import
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.c.laws.CTranslateLaws
import com.github.gchudnov.bscript.translator.laws.TypeInit

/**
 * Translates AST to Scala
 */
private[translator] final class CTranslator(laws: TranslateLaws, state: CState) extends Translator:
  override def fromAST(ast1: AST): Either[Throwable, String] =
    val cVisitor = CVisitor.make(laws)

    ast1
      .visit(state, cVisitor)
      .map(ss => ss.show())

  override inline def toAST[T](inline x: T): AST =
    Scala3Import.make(x)

object CTranslator {
  def make(meta: Meta, typeNames: TypeNames): Translator =
    val typeInit = CTypeInit
    val laws     = CTranslateLaws.make(typeNames, typeInit, meta)
    val state    = CState.make(meta)
    new CTranslator(laws, state)

}