package com.github.gchudnov.bscript.translator.internal.asm

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.LineOps
import com.github.gchudnov.bscript.translator.internal.scalax.ScalaState

import scala.collection.immutable.Seq


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
trait AsmStateT:
  def meta: Meta
  def lines: Seq[String]
  def imports: Set[String]
  def inits: Map[String, Seq[String]]

  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    val fmtInits   = inits.keys.toList.sorted.map(key => inits(key)).flatten
    LineOps.join(LineOps.joinNL(LineOps.joinNL(fmtImports, fmtInits), lines))

  def withLines(lines: Seq[String]): AsmState
  def withMeta(meta: Meta): AsmState
  def withImports(imports: Set[String]): AsmState
  def withInits(inits: Map[String, Seq[String]]): AsmState

/**
 * A state for C
 */
final case class AsmState(meta: Meta, lines: Seq[String], imports: Set[String], inits: Map[String, Seq[String]]) extends AsmStateT:

  override def withLines(lines: Seq[String]): AsmState =
    this.copy(lines = lines)

  override def withMeta(meta: Meta): AsmState =
    this.copy(meta = meta)

  override def withImports(imports: Set[String]): AsmState =
    this.copy(imports = imports)

  override def withInits(inits: Map[String, Seq[String]]): AsmState =
    this.copy(inits = inits)

/**
 * CState
 */
object AsmState:
  def make(meta: Meta): AsmState =
    new AsmState(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Map.empty[String, Seq[String]])
