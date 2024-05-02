package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.translator.laws.TypeInit

case object AsmTypeNA extends TypeInit:
  val voidType: String = "null"
  val boolType: String = "false"
  val i32Type: String = "I32.MIN_VALUE"
  val i64Type: String = "I64.MIN_VALUE"
  val f32Type: String = "F32.NaN"
  val f64Type: String = "F64.NaN"
  val decType: String = "F64.NaN"
  val dateType: String = "Date.parse(\"1900-01-01\")"
  val datetimeType: String = "Date.parse(\"1900-01-01\")"
  val strType: String = "\"!#\""
