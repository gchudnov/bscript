package com.github.gchudnov.bscript.builder.util

object Strings:

  def spaced(depth: Int): String =
    "  ".repeat(depth)

  def quoted(s: String): String =
    s"\"${s}\""

  def arrayAsString(xs: Iterable[String]): String =
    xs.mkString("[", ",", "]")
