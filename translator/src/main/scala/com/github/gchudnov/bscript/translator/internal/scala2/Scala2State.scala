package com.github.gchudnov.bscript.translator.internal.scala2

import com.github.gchudnov.bscript.lang.util.ShowOps

final case class Scala2State(lines: Seq[String]):
  def show(): String =
    ShowOps.join(lines)

object Scala2State:
  def make(): Scala2State =
    new Scala2State(lines = Vector.empty[String])
