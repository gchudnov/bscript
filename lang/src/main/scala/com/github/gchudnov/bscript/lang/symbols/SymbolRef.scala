package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * Reference to a Symbol
 * @param name
 *   name of the symbol
 */
final case class SymbolRef(name: String) extends Symbol

object SymbolRef:
  val auto: SymbolRef     = SymbolRef(TypeName.auto)
  val nothing: SymbolRef  = SymbolRef(TypeName.nothing)
  val void: SymbolRef     = SymbolRef(TypeName.void)
  val bool: SymbolRef     = SymbolRef(TypeName.bool)
  val i32: SymbolRef      = SymbolRef(TypeName.i32)
  val i64: SymbolRef      = SymbolRef(TypeName.i64)
  val f32: SymbolRef      = SymbolRef(TypeName.f32)
  val f64: SymbolRef      = SymbolRef(TypeName.f64)
  val dec: SymbolRef      = SymbolRef(TypeName.dec)
  val str: SymbolRef      = SymbolRef(TypeName.str)
  val date: SymbolRef     = SymbolRef(TypeName.date)
  val datetime: SymbolRef = SymbolRef(TypeName.datetime)
