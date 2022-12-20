package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * Reference to a type
 * @param name
 *   Name of the type that is defined in one of the scopes
 */
final case class TypeRef(name: String) extends Type

object TypeRef:
  val auto: TypeRef     = TypeRef(TypeName.auto)
  val nothing: TypeRef  = TypeRef(TypeName.nothing)
  val void: TypeRef     = TypeRef(TypeName.void)
  val bool: TypeRef     = TypeRef(TypeName.bool)
  val i32: TypeRef      = TypeRef(TypeName.i32)
  val i64: TypeRef      = TypeRef(TypeName.i64)
  val f32: TypeRef      = TypeRef(TypeName.f32)
  val f64: TypeRef      = TypeRef(TypeName.f64)
  val dec: TypeRef      = TypeRef(TypeName.dec)
  val str: TypeRef      = TypeRef(TypeName.str)
  val date: TypeRef     = TypeRef(TypeName.date)
  val datetime: TypeRef = TypeRef(TypeName.datetime)
