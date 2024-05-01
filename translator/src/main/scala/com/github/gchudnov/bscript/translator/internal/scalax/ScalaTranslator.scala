package com.github.gchudnov.bscript.translator.internal.scalax

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.{TranslateLaws, Translator}
import com.github.gchudnov.bscript.translator.internal.ScalaVisitor
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3Import
import com.github.gchudnov.bscript.translator.internal.scalax.ScalaState

/**
 * Translates AST to Scala
 */
private[translator] final class ScalaTranslator(laws: TranslateLaws, state: ScalaState) extends Translator:
  override def fromAST(ast1: AST): Either[Throwable, String] =
    val scalaVisitor = ScalaVisitor.make(laws)

    ast1
      .visit(state, scalaVisitor)
      .map(ss => ss.show())

  override inline def toAST[T](inline x: T): AST =
    Scala3Import.make(x)
