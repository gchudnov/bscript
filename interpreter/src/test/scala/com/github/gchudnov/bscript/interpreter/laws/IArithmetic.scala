package com.github.gchudnov.bscript.interpreter.laws

import com.github.gchudnov.bscript.interpreter.laws.Arithmetic
import com.github.gchudnov.bscript.interpreter.memory.*

final class IArithmetic() extends Arithmetic:

  override def add(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(IntCell(x + y))
    case (LongCell(x), LongCell(y))       => Right(LongCell(x + y))
    case (FloatCell(x), FloatCell(y))     => Right(FloatCell(x + y))
    case (DoubleCell(x), DoubleCell(y))   => Right(DoubleCell(x + y))
    case (DecimalCell(x), DecimalCell(y)) => Right(DecimalCell(x + y))
    case (StrCell(x), StrCell(y))         => Right(StrCell(x + y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval Add(${a}, ${b})"))

  override def sub(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(IntCell(x - y))
    case (LongCell(x), LongCell(y))       => Right(LongCell(x - y))
    case (FloatCell(x), FloatCell(y))     => Right(FloatCell(x - y))
    case (DoubleCell(x), DoubleCell(y))   => Right(DoubleCell(x - y))
    case (DecimalCell(x), DecimalCell(y)) => Right(DecimalCell(x - y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval Sub(${a}, ${b})"))

  override def mul(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(IntCell(x * y))
    case (LongCell(x), LongCell(y))       => Right(LongCell(x * y))
    case (FloatCell(x), FloatCell(y))     => Right(FloatCell(x * y))
    case (DoubleCell(x), DoubleCell(y))   => Right(DoubleCell(x * y))
    case (DecimalCell(x), DecimalCell(y)) => Right(DecimalCell(x * y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval Mul(${a}, ${b})"))

  override def div(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(IntCell(x / y))
    case (LongCell(x), LongCell(y))       => Right(LongCell(x / y))
    case (FloatCell(x), FloatCell(y))     => Right(FloatCell(x / y))
    case (DoubleCell(x), DoubleCell(y))   => Right(DoubleCell(x / y))
    case (DecimalCell(x), DecimalCell(y)) => Right(DecimalCell(x / y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval Div(${a}, ${b})"))

  override def mod(lhs: Cell, rhs: Cell): Either[Throwable, Cell] = (lhs, rhs) match
    case (IntCell(x), IntCell(y))         => Right(IntCell(x % y))
    case (LongCell(x), LongCell(y))       => Right(LongCell(x % y))
    case (FloatCell(x), FloatCell(y))     => Right(FloatCell(x % y))
    case (DoubleCell(x), DoubleCell(y))   => Right(DoubleCell(x % y))
    case (DecimalCell(x), DecimalCell(y)) => Right(DecimalCell(x % y))
    case (a, b)                           => Left(new MemoryException(s"Cannot eval Mod(${a}, ${b})"))

  override def unaryMinus(value: Cell): Either[Throwable, Cell] = value match
    case IntCell(x)    => Right(IntCell(-x))
    case LongCell(x)   => Right(LongCell(-x))
    case FloatCell(x)  => Right(FloatCell(-x))
    case DoubleCell(x) => Right(DoubleCell(-x))
    case a             => Left(new MemoryException(s"Cannot eval UnaryMinus(${a})"))
