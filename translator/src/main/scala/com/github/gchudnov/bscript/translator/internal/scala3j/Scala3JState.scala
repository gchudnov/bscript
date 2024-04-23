package com.github.gchudnov.bscript.translator.internal.scala3j

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.translator.internal.ScalaState
import scala.collection.immutable.Seq

/**
 * Scala 3 State with Java Types (to allow 'null')
 */
final case class Scala3JState(meta: Meta, lines: Seq[String], imports: Set[String], inits: Map[String, Seq[String]]) extends ScalaState:

  override def withLines(lines: Seq[String]): ScalaState =
    this.copy(lines = lines)

  override def withMeta(meta: Meta): ScalaState =
    this.copy(meta = meta)

  override def withImports(imports: Set[String]): ScalaState =
    this.copy(imports = imports)

  override def withInits(inits: Map[String, Seq[String]]): ScalaState =
    this.copy(inits = inits)

object Scala3JState:
  def make(meta: Meta): Scala3JState =
    new Scala3JState(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Map.empty[String, Seq[String]])
