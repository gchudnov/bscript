package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * A built-in type, e.g. int, long, float
 *
 * @param name
 *   name of the type
 */
final case class SType(name: String) extends Symbol with Type

object SType:
  val auto     = SType(TypeName.auto)
  val nothing  = SType(TypeName.nothing)
  val void     = SType(TypeName.void)
  val bool     = SType(TypeName.bool)
  val i32      = SType(TypeName.i32)
  val i64      = SType(TypeName.i64)
  val f32      = SType(TypeName.f32)
  val f64      = SType(TypeName.f64)
  val dec      = SType(TypeName.dec)
  val str      = SType(TypeName.str)
  val date     = SType(TypeName.date)
  val datetime = SType(TypeName.datetime)
