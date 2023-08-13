package com.github.gchudnov.bscript.interpreter.util

import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.lang.const.NothingVal

/**
 * Converts an AST-constant to a Cell.
 */
private[interpreter] object ConstConv:
  def toCell(c: Const): Cell =
    c match
      case BoolVal(v)     => Cell.Bool(v)
      case ByteVal(v)     => Cell.U8(v)
      case CharVal(v)     => Cell.Chr(v)
      case DateTimeVal(v) => Cell.DateTime(v)
      case DateVal(v)     => Cell.Date(v)
      case DecVal(v)      => Cell.Dec(v)
      case DoubleVal(v)   => Cell.F64(v)
      case FloatVal(v)    => Cell.F32(v)
      case IntVal(v)      => Cell.I32(v)
      case LongVal(v)     => Cell.I64(v)
      case NothingVal     => Cell.Nothing
      case ShortVal(v)    => Cell.I16(v)
      case StrVal(v)      => Cell.Str(v)
      case VoidVal        => Cell.Void
