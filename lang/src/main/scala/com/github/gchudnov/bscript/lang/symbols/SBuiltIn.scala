package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * A built-in type, e.g. int, long, float
 *
 * @param name
 *   name of the type
 */
final case class SBuiltIn(name: String) extends Symbol with Type

object SBuiltIn:
  val auto     = SBuiltIn(TypeName.auto)
  val nothing  = SBuiltIn(TypeName.nothing)
  val void     = SBuiltIn(TypeName.void)
  val bool     = SBuiltIn(TypeName.bool)
  val i32      = SBuiltIn(TypeName.i32)
  val i64      = SBuiltIn(TypeName.i64)
  val f32      = SBuiltIn(TypeName.f32)
  val f64      = SBuiltIn(TypeName.f64)
  val dec      = SBuiltIn(TypeName.dec)
  val str      = SBuiltIn(TypeName.str)
  val date     = SBuiltIn(TypeName.date)
  val datetime = SBuiltIn(TypeName.datetime)
