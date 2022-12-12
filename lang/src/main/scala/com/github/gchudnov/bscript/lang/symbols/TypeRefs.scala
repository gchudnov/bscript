package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeNames

object TypeRefs:
  val auto: TypeRef     = TypeRef(TypeNames.auto)
  val nothing: TypeRef  = TypeRef(TypeNames.nothing)
  val void: TypeRef     = TypeRef(TypeNames.void)
  val bool: TypeRef     = TypeRef(TypeNames.bool)
  val i32: TypeRef      = TypeRef(TypeNames.i32)
  val i64: TypeRef      = TypeRef(TypeNames.i64)
  val f32: TypeRef      = TypeRef(TypeNames.f32)
  val f64: TypeRef      = TypeRef(TypeNames.f64)
  val dec: TypeRef      = TypeRef(TypeNames.dec)
  val str: TypeRef      = TypeRef(TypeNames.str)
  val date: TypeRef     = TypeRef(TypeNames.date)
  val datetime: TypeRef = TypeRef(TypeNames.datetime)
