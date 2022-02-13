package com.github.gchudnov.bscript.b1.internal.laws

import com.github.gchudnov.bscript.interpreter.laws.Comparator
import com.github.gchudnov.bscript.interpreter.memory.*

final class B1Comparator() extends Comparator:

  override def less(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x < y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x < y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x < y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x < y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x < y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x < y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval less(${a}, ${b})"))

  override def lessEqual(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x <= y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x <= y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x <= y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x <= y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x <= y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x <= y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval lessEqual(${a}, ${b})"))

  override def greater(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x > y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x > y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x > y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x > y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x > y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x > y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval greater(${a}, ${b})"))

  override def greaterEqual(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x >= y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x >= y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x >= y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x >= y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x >= y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x >= y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval greaterEqual(${a}, ${b})"))

  override def equal(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x == y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x == y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x == y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x == y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x == y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x == y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval equal(${a}, ${b})"))

  override def notEqual(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(BoolCell(x != y))
    case (LongCell(x), LongCell(y))       => Right(BoolCell(x != y))
    case (FloatCell(x), FloatCell(y))     => Right(BoolCell(x != y))
    case (DoubleCell(x), DoubleCell(y))   => Right(BoolCell(x != y))
    case (DecimalCell(x), DecimalCell(y)) => Right(BoolCell(x != y))
    case (StrCell(x), StrCell(y))         => Right(BoolCell(x != y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval notEqual(${a}, ${b})"))
