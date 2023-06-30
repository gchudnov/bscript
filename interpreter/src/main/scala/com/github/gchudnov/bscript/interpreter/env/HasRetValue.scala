package com.github.gchudnov.bscript.interpreter.env

import com.github.gchudnov.bscript.interpreter.memory.Cell

/**
 * HasRetValue
 */
trait HasRetValue:
  def retValue: Cell

object HasRetValue:
  def apply(a: Cell): HasRetValue = new HasRetValue:
    override def retValue: Cell = a
