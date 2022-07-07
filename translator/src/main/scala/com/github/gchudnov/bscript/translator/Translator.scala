package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3Visitor
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3TranslateLaws
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JTranslateLaws
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.laws.TypeInit

trait Translator:

  def translateScala3(ast1: AST, meta1: Meta, typeNames: TypeNames, typeInit: TypeInit): Either[Throwable, String] =
    val laws = Scala3TranslateLaws.make(typeNames, typeInit, meta1)
    translateScala(ast1, meta1, laws)

  def translateScala3J(ast1: AST, meta1: Meta, typeNames: TypeNames, typeInit: TypeInit): Either[Throwable, String] =
    val laws = Scala3JTranslateLaws.make(typeNames, typeInit, meta1)
    translateScala(ast1, meta1, laws)

  private def translateScala(ast1: AST, meta1: Meta, laws: TranslateLaws): Either[Throwable, String] =
    val scalaVisitor = Scala3Visitor.make(laws)
    val scalaState   = Scala3State.make(meta1)

    ast1
      .visit(scalaState, scalaVisitor)
      .map(ss => ss.show())

object Translator extends Translator
