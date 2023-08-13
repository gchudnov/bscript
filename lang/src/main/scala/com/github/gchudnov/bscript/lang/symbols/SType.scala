package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * A built-in type, e.g. int, long, float
 *
 * @param name
 *   name of the type
 */
final case class SType(name: String) extends Symbol

object SType:
  val nothing  = SType(TypeName.nothing)
  val void     = SType(TypeName.void)
  val bool     = SType(TypeName.bool)
  val u8       = SType(TypeName.u8)
  val i16      = SType(TypeName.i16)
  val i32      = SType(TypeName.i32)
  val i64      = SType(TypeName.i64)
  val f32      = SType(TypeName.f32)
  val f64      = SType(TypeName.f64)
  val dec      = SType(TypeName.dec)
  val chr      = SType(TypeName.chr)
  val str      = SType(TypeName.str)
  val date     = SType(TypeName.date)
  val datetime = SType(TypeName.datetime)

  def parse(name: String): Option[SType] =
    name match
      case TypeName.nothing  => Some(nothing)
      case TypeName.void     => Some(void)
      case TypeName.bool     => Some(bool)
      case TypeName.u8       => Some(u8)
      case TypeName.i16      => Some(i16)
      case TypeName.i32      => Some(i32)
      case TypeName.i64      => Some(i64)
      case TypeName.f32      => Some(f32)
      case TypeName.f64      => Some(f64)
      case TypeName.dec      => Some(dec)
      case TypeName.chr      => Some(chr)
      case TypeName.str      => Some(str)
      case TypeName.date     => Some(date)
      case TypeName.datetime => Some(datetime)
      case _                 => None
