package com.github.gchudnov.bscript.lang.const

import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.const.NothingVal

/**
 * Any Constant Variable
 */
trait Const

object Const:

  def toTypeAST(const: Const): TypeAST =
    const match
      case _: BoolVal         => BuiltInType(TypeName.bool)
      case _: ByteVal         => BuiltInType(TypeName.u8)
      case _: CharVal         => BuiltInType(TypeName.chr)
      case _: DateTimeVal     => BuiltInType(TypeName.datetime)
      case _: DateVal         => BuiltInType(TypeName.date)
      case _: DecVal          => BuiltInType(TypeName.dec)
      case _: DoubleVal       => BuiltInType(TypeName.f64)
      case _: FloatVal        => BuiltInType(TypeName.f32)
      case _: IntVal          => BuiltInType(TypeName.i32)
      case _: LongVal         => BuiltInType(TypeName.i64)
      case _: NothingVal.type => BuiltInType(TypeName.nothing)
      case _: ShortVal        => BuiltInType(TypeName.i16)
      case _: StrVal          => BuiltInType(TypeName.str)
      case _: VoidVal.type    => BuiltInType(TypeName.void)
