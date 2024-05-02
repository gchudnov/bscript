package com.github.gchudnov.bscript.translator.into.asm.stdlib

import com.github.gchudnov.bscript.lang.types.TypeNames

/**
 * Magic values that specify an undefined value for a type
 */
object Undefined {
  def magic(typeNames: TypeNames): Map[String, String] =
    Map(
      typeNames.strType -> """"!#"""",
      typeNames.i32Type -> "i32.MIN_VALUE",
      typeNames.i64Type -> "i64.MIN_VALUE",
      typeNames.f32Type -> "f32.NaN",
      typeNames.f64Type -> "f64.NaN",
      typeNames.dateType -> """Date.parse("1900-01-01")""",
      typeNames.datetimeType -> """Date.parse("1900-01-01")"""
    )
}
