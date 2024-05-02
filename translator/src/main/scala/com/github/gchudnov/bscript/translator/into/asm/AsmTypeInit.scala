package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.translator.laws.TypeInit

case object AsmTypeInit extends TypeInit:
  val voidType: String = "void"
  val boolType: String = "false"
  val i32Type: String  = "0"
  val i64Type: String  = "0L"
  val f32Type: String  = "0.0f"
  val f64Type: String  = "0.0"
  val decType: String  = "0.0"
  val dateType: String = "Date.parse(\"1900-01-01\")"
  val datetimeType: String = "Date.parse(\"1900-01-01\")"
  val strType: String  = "\"\""
