package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.LineOps

final case class Scala3State(meta: Meta, lines: Seq[String], imports: Set[String]):
  def show(): String =
    val fmtImports = imports.toSeq.sorted.map(i => s"import $i")
    val pad        = if (fmtImports.nonEmpty) then Seq("") else Seq.empty[String]
    LineOps.join(fmtImports ++ pad ++ lines)

object Scala3State:
  def make(meta: Meta): Scala3State =
    new Scala3State(meta = meta, lines = Vector.empty[String], imports = Set.empty[String])
