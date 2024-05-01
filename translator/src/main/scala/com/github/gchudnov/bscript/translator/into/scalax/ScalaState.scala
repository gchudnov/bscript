package com.github.gchudnov.bscript.translator.into.scalax

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.LineOps

import scala.collection.immutable.Seq

/**
 * {{{
 *   The code after converting to Scala, will be structured like this:
 *
 *   {{{
 *     import ...
 *     import ...
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
trait ScalaState:
  def meta: Meta
  def lines: Seq[String]
  def imports: Set[String]
  def inits: Map[String, Seq[String]]

  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    val fmtInits   = inits.keys.toList.sorted.map(key => inits(key)).flatten
    LineOps.join(LineOps.joinNL(LineOps.joinNL(fmtImports, fmtInits), lines))

  def withLines(lines: Seq[String]): ScalaState
  def withMeta(meta: Meta): ScalaState
  def withImports(imports: Set[String]): ScalaState
  def withInits(inits: Map[String, Seq[String]]): ScalaState
