package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.LineOps

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
 */
final case class Scala3State(meta: Meta, lines: Seq[String], imports: Set[String], inits: Seq[String]):
  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    LineOps.join(LineOps.joinNL(LineOps.joinNL(fmtImports, inits), lines))

object Scala3State:
  def make(meta: Meta): Scala3State =
    new Scala3State(meta = meta, lines = Vector.empty[String], imports = Set.empty[String], inits = Seq.empty[String])
