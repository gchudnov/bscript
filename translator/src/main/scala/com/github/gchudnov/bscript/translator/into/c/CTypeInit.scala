package com.github.gchudnov.bscript.translator.into.c

import com.github.gchudnov.bscript.translator.laws.TypeInit

case object CTypeInit extends TypeInit:
  val voidType: String = "void"
  val boolType: String = "false"
  val i32Type: String  = "0"
  val i64Type: String  = "0L"
  val f32Type: String  = "0.0f"
  val f64Type: String  = "0.0"
  val decType: String  = "0.0"
  val strType: String  = "\"\""
