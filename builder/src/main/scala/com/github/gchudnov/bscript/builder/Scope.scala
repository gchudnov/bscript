package com.github.gchudnov.bscript.builder

trait Scope:
  def name: String

final case class ScopeRef(name: String) extends Scope
