package com.github.gchudnov.bscript.translator.internal.c

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.LineOps

import scala.collection.immutable.Seq
import com.github.gchudnov.bscript.translator.internal.ScalaState


/**
 * {{{
 *   The code after converting to C, will be structured like this:
 *
 *   {{{
 *     #include ...
 *     #include ...
 *
 *     init
 *     init
 *
 *     line
 *     line
 * }}}
 *
 * Here 'init' are the additional code lines, e.g. implicits and 'line' are the application-lines.
 *
 * NOTE: it is important to have a dedicated types for Scala3 and Scala3J since using these case classes we differentiate between these cases in `prelude` generation.
 */
trait CStateT:
  def meta: Meta
  def lines: Seq[String]
  def imports: Set[String]
  def inits: Map[String, Seq[String]]

  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"#include $i")
    val fmtInits   = inits.keys.toList.sorted.map(key => inits(key)).flatten
    LineOps.join(LineOps.joinNL(LineOps.joinNL(fmtImports, fmtInits), lines))

  def withLines(lines: Seq[String]): CState
  def withMeta(meta: Meta): CState
  def withImports(imports: Set[String]): CState
  def withInits(inits: Map[String, Seq[String]]): CState

/**
 * A state for C
 */
final case class CState(meta: Meta, lines: Seq[String], imports: Set[String], inits: Map[String, Seq[String]]) extends CStateT:

  override def withLines(lines: Seq[String]): CState =
    this.copy(lines = lines)

  override def withMeta(meta: Meta): CState =
    this.copy(meta = meta)

  override def withImports(imports: Set[String]): CState =
    this.copy(imports = imports)

  override def withInits(inits: Map[String, Seq[String]]): CState =
    this.copy(inits = inits)

/**
 * CState
 */
object CState:
  def make(meta: Meta): CState =
    new CState(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Map.empty[String, Seq[String]])
