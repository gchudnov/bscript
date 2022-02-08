package com.github.gchudnov.bscript.interpreter.laws

import com.github.gchudnov.bscript.interpreter.memory.Cell

trait BoolArithmetic:
  def not(value: Cell): Either[Throwable, Cell]
  def and(lhs: Cell, rhs: Cell): Either[Throwable, Cell]
  def or(lhs: Cell, rhs: Cell): Either[Throwable, Cell]
