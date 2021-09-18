package com.github.gchudnov.bscript.lang.util

/**
 * Name generator
 */
case class Gen(seed: Long):
  def name(): (Gen, String) =
    (Gen(seed + 1), Base26.encode(seed))

object Gen:
  val empty: Gen =
    Gen(0L)
