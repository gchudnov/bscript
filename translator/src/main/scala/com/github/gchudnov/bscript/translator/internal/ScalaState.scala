package com.github.gchudnov.bscript.translator.internal

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
trait ScalaState:
  def meta: Meta
  def lines: Seq[String]
  def imports: Set[String]
  def inits: Seq[String]

  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    LineOps.join(LineOps.joinNL(LineOps.joinNL(fmtImports, inits), lines))
