package com.github.gchudnov.bscript.b1.internal.laws

import com.github.gchudnov.bscript.interpreter.laws.TypeCaster
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.symbols.Type

final class B1BasicTypeCaster(types: Types) extends TypeCaster:

  private val autoTypeName: String     = types.autoType.name
  private val voidTypeName: String     = types.voidType.name
  private val boolTypeName: String     = types.boolType.name
  private val i32TypeName: String      = types.i32Type.name
  private val i64TypeName: String      = types.i64Type.name
  private val f32TypeName: String      = types.f32Type.name
  private val f64TypeName: String      = types.f64Type.name
  private val decTypeName: String      = types.decType.name
  private val strTypeName: String      = types.strType.name
  private val dateTypeName: String     = types.dateType.name
  private val datetimeTypeName: String = types.datetimeType.name

  override def cast(value: Cell, toType: Type): Either[MemoryException, Cell] = value match

    case NothingCell =>
      Right(NothingCell) // Nothing is always Nothing when we cast it to some value

    case VoidCell =>
      toType.name match
        case `voidTypeName` => Right(VoidCell)
        case `autoTypeName` => Right(VoidCell)
        case other          => castError(value, other)

    case BoolCell(x) =>
      toType.name match
        case `autoTypeName` => Right(BoolCell(x))
        case `voidTypeName` => Right(VoidCell)
        case `boolTypeName` => Right(BoolCell(x))
        case other          => castError(value, other)

    case IntCell(x) =>
      toType.name match
        case `autoTypeName` => Right(IntCell(x))
        case `i32TypeName`  => Right(IntCell(x))
        case `i64TypeName`  => Right(LongCell(x.toLong))
        case `f32TypeName`  => Right(FloatCell(x.toFloat))
        case `f64TypeName`  => Right(DoubleCell(x.toDouble))
        case `decTypeName`  => Right(DecimalCell(BigDecimal(x)))
        case other          => castError(value, other)

    case LongCell(x) =>
      toType.name match
        case `autoTypeName` => Right(LongCell(x))
        case `i64TypeName`  => Right(LongCell(x))
        case `f32TypeName`  => Right(FloatCell(x.toFloat))
        case `f64TypeName`  => Right(DoubleCell(x.toDouble))
        case `decTypeName`  => Right(DecimalCell(BigDecimal(x)))
        case other          => castError(value, other)

    case FloatCell(x) =>
      toType.name match
        case `autoTypeName` => Right(FloatCell(x))
        case `f32TypeName`  => Right(FloatCell(x))
        case `f64TypeName`  => Right(DoubleCell(x.toDouble))
        case `decTypeName`  => Right(DecimalCell(BigDecimal(x)))
        case other          => castError(value, other)

    case DoubleCell(x) =>
      toType.name match
        case `autoTypeName` => Right(DoubleCell(x))
        case `f64TypeName`  => Right(DoubleCell(x))
        case `decTypeName`  => Right(DecimalCell(BigDecimal(x)))
        case other          => castError(value, other)

    case DecimalCell(x) =>
      toType.name match
        case `autoTypeName` => Right(DecimalCell(x))
        case `decTypeName`  => Right(DecimalCell(x))
        case other          => castError(value, other)

    case StrCell(x) =>
      toType.name match
        case `autoTypeName` => Right(StrCell(x))
        case `strTypeName`  => Right(StrCell(x))
        case other          => castError(value, other)

    case DateCell(x) =>
      toType.name match
        case `autoTypeName` => Right(DateCell(x))
        case `dateTypeName` => Right(DateCell(x))
        case other          => castError(value, other)

    case DateTimeCell(x) =>
      toType.name match
        case `autoTypeName`     => Right(DateTimeCell(x))
        case `dateTypeName`     => Right(DateCell(x.toLocalDate))
        case `datetimeTypeName` => Right(DateTimeCell(x))
        case other              => castError(value, other)

  private def castError[A, B](value: A, other: B): Either[MemoryException, Cell] =
    Left(new MemoryException(s"Cannot cast '${value}' to '${other}'"))
