package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types

/**
 * Rewriter Type Names
 */
private[rewriter] object RTypeNames:

  def make(): TypeNames = new TypeNames:
    override def autoType: String     = "auto"
    override def nothingType: String  = "nothing"
    override def voidType: String     = "void"
    override def boolType: String     = "bool"
    override def i32Type: String      = "i32"
    override def i64Type: String      = "i64"
    override def f32Type: String      = "f32"
    override def f64Type: String      = "f64"
    override def decType: String      = "dec"
    override def strType: String      = "str"
    override def dateType: String     = "date"
    override def datetimeType: String = "datetime"
