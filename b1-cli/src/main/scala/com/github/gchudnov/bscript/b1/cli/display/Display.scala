package com.github.gchudnov.bscript.b1.cli.display

object Display:
  val lineWidth = 64

  def padRight(value: String, len: Int): String =
    if (value.length < len) then value + " " * (len - value.length)
    else value
