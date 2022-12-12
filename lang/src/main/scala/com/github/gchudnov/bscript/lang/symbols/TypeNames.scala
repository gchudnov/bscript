package com.github.gchudnov.bscript.lang.types

/**
 * Type Names.
 *
 * Used to allow to specify different type names for a type. E.g. integer data type might be i32 or int.
 */
object TypeNames:
  val auto: String     = "auto"
  val nothing: String  = "nothing"
  val void: String     = "void"
  val bool: String     = "bool"
  val i32: String      = "i32"
  val i64: String      = "i64"
  val f32: String      = "f32"
  val f64: String      = "f64"
  val dec: String      = "dec" // decimal
  val str: String      = "str"
  val date: String     = "date"
  val datetime: String = "datetime"

  def all: List[String] = List(
    auto,
    nothing,
    void,
    bool,
    i32,
    i64,
    f32,
    f64,
    dec,
    str,
    date,
    datetime
  )
