package com.github.gchudnov.bscript.lang.calc

import com.github.gchudnov.bscript.lang.memory.{ BoolCell, Cell, MemoryException }

trait BoolArithmetic:
  def not(value: Cell): Either[Throwable, Cell]
  def and(lhs: Cell, rhs: Cell): Either[Throwable, Cell]
  def or(lhs: Cell, rhs: Cell): Either[Throwable, Cell]

final class BuiltInBoolArithmetic() extends BoolArithmetic:
  override def not(value: Cell): Either[Throwable, Cell] = value match
    case BoolCell(x) => Right(BoolCell(!x))
    case a           => Left(new MemoryException(s"Cannot eval Not('${a}')"))

  override def and(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (BoolCell(x), BoolCell(y)) => Right(BoolCell(x && y))
    case (a, b)                     => Left(new MemoryException(s"Cannot eval Add(${a}, ${b})"))

  override def or(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (BoolCell(x), BoolCell(y)) => Right(BoolCell(x || y))
    case (a, b)                     => Left(new MemoryException(s"Cannot eval Or(${a}, ${b})"))
