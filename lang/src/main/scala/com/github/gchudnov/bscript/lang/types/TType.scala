package com.github.gchudnov.bscript.lang.types

/**
 * Type of a symbol.
 *
 * Usage:
 * {{{
 * TType -+
 *        |
 *        +- TBuiltIn
 *        +- TMethod
 *        +- TStruct
 *        |
 *        +- TApplied          // TODO: not supported at the moment
 *        +- TAndOr -+- TAnd   // TODO: not supported at the moment
 *                   +- TOr    // TODO: not supported at the moment
 *
 * }}}
 */
trait TType


object TType:
  val nothing  = TBuiltIn(TypeName.nothing)
  val void     = TBuiltIn(TypeName.void)
  val bool     = TBuiltIn(TypeName.bool)
  val i8       = TBuiltIn(TypeName.i8)
  val i16      = TBuiltIn(TypeName.i16)
  val i32      = TBuiltIn(TypeName.i32)
  val i64      = TBuiltIn(TypeName.i64)
  val f32      = TBuiltIn(TypeName.f32)
  val f64      = TBuiltIn(TypeName.f64)
  val dec      = TBuiltIn(TypeName.dec)
  val chr      = TBuiltIn(TypeName.chr)
  val str      = TBuiltIn(TypeName.str)
  val date     = TBuiltIn(TypeName.date)
  val datetime = TBuiltIn(TypeName.datetime)

  def parse(name: String): Option[TBuiltIn] =
    name match
      case TypeName.nothing  => Some(nothing)
      case TypeName.void     => Some(void)
      case TypeName.bool     => Some(bool)
      case TypeName.i8       => Some(i8)
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
