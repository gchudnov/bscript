package com.github.gchudnov.bscript.translator.into.scala3

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.translator.into.scalax.ScalaState
import scala.collection.immutable.Seq

/**
 * A state for Scala 3
 */
final case class Scala3State(meta: Meta, lines: Seq[String], imports: Set[String], inits: Map[String, Seq[String]]) extends ScalaState:

  override def withLines(lines: Seq[String]): ScalaState =
    this.copy(lines = lines)

  override def withMeta(meta: Meta): ScalaState =
    this.copy(meta = meta)

  override def withImports(imports: Set[String]): ScalaState =
    this.copy(imports = imports)

  override def withInits(inits: Map[String, Seq[String]]): ScalaState =
    this.copy(inits = inits)

object Scala3State:
  def make(meta: Meta): Scala3State =
    new Scala3State(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Map.empty[String, Seq[String]])
