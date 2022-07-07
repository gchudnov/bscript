package com.github.gchudnov.bscript.translator.internal.scala3j

import com.github.gchudnov.bscript.translator.laws.TypeInit

case object Scala3JTypeInit extends TypeInit:
  val voidType: String = "()"
  val boolType: String = "false"
  val i32Type: String  = "0"
  val i64Type: String  = "0L"
  val f32Type: String  = "0.0f"
  val f64Type: String  = "0.0"
  val decType: String  = "BigDecimal.valueOf(0)"
  val strType: String  = "\"\""
