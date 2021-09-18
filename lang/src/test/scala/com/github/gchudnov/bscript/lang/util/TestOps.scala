package com.github.gchudnov.bscript.lang.util

object TestOps:
  def dehydrate(s: String): String =
    s.replaceAll("\\s", "")
