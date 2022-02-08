package com.github.gchudnov.bscript.serde

import com.github.gchudnov.bscript.lang.types.TypeNames

object SGlobals:

  val typeNames: TypeNames = new TypeNames:
    override def autoType: String     = "auto"
    override def nothingType: String  = "nothing"
    override def voidType: String     = "void"
    override def boolType: String     = "boolean"
    override def i32Type: String      = "int"
    override def i64Type: String      = "long"
    override def f32Type: String      = "float"
    override def f64Type: String      = "double"
    override def decType: String      = "decimal"
    override def strType: String      = "string"
    override def dateType: String     = "date"
    override def datetimeType: String = "datetime"
