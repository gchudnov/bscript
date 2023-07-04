package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeSymbols

trait HasScopeSymbols:
  def scopeSymbols: ScopeSymbols

object HasScopeSymbols:
  def apply(ss: ScopeSymbols): HasScopeSymbols = new HasScopeSymbols:
    override val scopeSymbols: ScopeSymbols = ss
