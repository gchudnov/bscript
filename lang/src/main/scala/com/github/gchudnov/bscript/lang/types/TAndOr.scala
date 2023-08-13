package com.github.gchudnov.bscript.lang.types

/**
 * Either And or Or type.
 */
trait TAndOr extends TType:
  def left: TType
  def right: TType
