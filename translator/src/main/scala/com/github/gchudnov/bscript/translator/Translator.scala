package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.translator.internal.scala3.ScalaVisitor
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JState
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3TranslateLaws
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JTranslateLaws
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.laws.TypeInit
import com.github.gchudnov.bscript.translator.internal.ScalaState

trait Translator:

  def translateScala3(ast1: AST, meta1: Meta, typeNames: TypeNames, typeInit: TypeInit): Either[Throwable, String] =
    val laws = Scala3TranslateLaws.make(typeNames, typeInit, meta1)
    val state = Scala3State.make(meta1)
    translateScala(ast1, laws, state)

  def translateScala3J(ast1: AST, meta1: Meta, typeNames: TypeNames, typeInit: TypeInit): Either[Throwable, String] =
    val laws = Scala3JTranslateLaws.make(typeNames, typeInit, meta1)
    val state = Scala3JState.make(meta1)
    translateScala(ast1, laws, state)

  private def translateScala(ast1: AST, laws: TranslateLaws, state: ScalaState): Either[Throwable, String] =
    val scalaVisitor = ScalaVisitor.make(laws)

    ast1
      .visit(state, scalaVisitor)
      .map(ss => ss.show())

object Translator extends Translator
