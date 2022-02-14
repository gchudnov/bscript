package com.github.gchudnov.bscript.translator.internal.scala2

import com.github.gchudnov.bscript.lang.util.LineOps

final case class Scala2State(lines: Seq[String], imports: Set[String]):
  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    val pad        = if (fmtImports.nonEmpty) then Seq("") else Seq.empty[String]
    LineOps.join(fmtImports ++ pad ++ lines)

object Scala2State:
  def make(): Scala2State =
    new Scala2State(lines = Vector.empty[String], imports = Set.empty[String])
