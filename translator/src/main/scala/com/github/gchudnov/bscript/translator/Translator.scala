package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2Visitor
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.translator.internal.scala2.ScalaTranslateLaws
import com.github.gchudnov.bscript.translator.internal.scala2j.ScalaJTranslateLaws
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames

trait Translator:

  def translateScala(ast1: AST, meta1: Meta, typeNames: TypeNames): Either[Throwable, String] =
    val laws = ScalaTranslateLaws.make(typeNames, meta1)
    translateScala(ast1, meta1, laws)

  def translateScalaJ(ast1: AST, meta1: Meta, typeNames: TypeNames): Either[Throwable, String] =
    val laws = ScalaJTranslateLaws.make(typeNames, meta1)
    translateScala(ast1, meta1, laws)

  private def translateScala(ast1: AST, meta1: Meta, laws: TranslateLaws): Either[Throwable, String] =
    val scalaVisitor = Scala2Visitor.make(laws)
    val scalaState   = Scala2State.make(meta1)

    ast1
      .visit(scalaState, scalaVisitor)
      .map(ss => ss.show())

object Translator extends Translator
