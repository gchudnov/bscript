package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeNames

object SymbolRefs:
  val auto: SymbolRef     = SymbolRef(TypeNames.auto)
  val nothing: SymbolRef  = SymbolRef(TypeNames.nothing)
  val void: SymbolRef     = SymbolRef(TypeNames.void)
  val bool: SymbolRef     = SymbolRef(TypeNames.bool)
  val i32: SymbolRef      = SymbolRef(TypeNames.i32)
  val i64: SymbolRef      = SymbolRef(TypeNames.i64)
  val f32: SymbolRef      = SymbolRef(TypeNames.f32)
  val f64: SymbolRef      = SymbolRef(TypeNames.f64)
  val dec: SymbolRef      = SymbolRef(TypeNames.dec)
  val str: SymbolRef      = SymbolRef(TypeNames.str)
  val date: SymbolRef     = SymbolRef(TypeNames.date)
  val datetime: SymbolRef = SymbolRef(TypeNames.datetime)
