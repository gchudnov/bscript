package com.github.gchudnov.bscript.translator.internal.scala3j

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.translator.internal.ScalaState

/**
 * Scala 3 State with Java Types (to allow 'null')
 */
final case class Scala3JState(meta: Meta, lines: Seq[String], imports: Set[String], inits: Seq[String]) extends ScalaState

object Scala3JState:
  def make(meta: Meta): Scala3JState =
    new Scala3JState(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Seq.empty[String])
