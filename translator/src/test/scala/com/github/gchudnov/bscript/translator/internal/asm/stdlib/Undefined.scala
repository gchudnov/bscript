package com.github.gchudnov.bscript.translator.internal.asm.stdlib

object Undefined {
  val undefinedStr: String = "!#"
  val undefinedI32: String = "i32.MIN_VALUE"
  val undefinedI64: String = "i64.MIN_VALUE"
  val undefinedF32: String = "f32.NaN"
  val undefinedF64: String = "f64.NaN"
}
