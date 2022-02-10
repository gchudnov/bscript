package com.github.gchudnov.bscript.b1

final case class B1Options(
  hasPrelude: Boolean = true
):
  def withPrelude(hasPrelude: Boolean): B1Options = copy(hasPrelude = hasPrelude)

object B1Options:
  val default =
    B1Options()
