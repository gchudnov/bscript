package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2Visitor
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.translator.internal.scala2.ScalaTranslateLaws
import com.github.gchudnov.bscript.translator.internal.scala2.ScalaJTranslateLaws
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames

trait Translator:

  def translateScala(ast1: AST, typeNames: TypeNames, meta: Meta): Either[Throwable, String] =
    val laws = ScalaTranslateLaws.make(typeNames, meta)
    translateScala(ast1, laws)

  def translateScalaJ(ast1: AST, typeNames: TypeNames, meta: Meta): Either[Throwable, String] =
    val laws = ScalaJTranslateLaws.make(typeNames, meta)
    translateScala(ast1, laws)

  def translateScala(ast1: AST, laws: TranslateLaws): Either[Throwable, String] =
    val scalaVisitor = Scala2Visitor.make(laws)
    val scalaState   = Scala2State.make()

    ast1
      .visit(scalaState, scalaVisitor)
      .map(ss => ss.show())

object Translator extends Translator
