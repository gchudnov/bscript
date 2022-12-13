package com.github.gchudnov.bscript.lang.symbols

final case class ScopeRef(name: String) extends Scope

object ScopeRef:
  val global: ScopeRef = ScopeRef("#global")
