package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ReadScopeSymbols
import com.github.gchudnov.bscript.builder.state.WriteScopeSymbols

trait HasReadScopeSymbols:
  def scopeSymbols: ReadScopeSymbols

trait HasWriteScopeSymbols:
  def scopeSymbols: WriteScopeSymbols

trait HasScopeSymbols:
  def scopeSymbols: ScopeSymbols

object HasScopeSymbols:
  def apply(ss: ScopeSymbols): HasScopeSymbols = new HasScopeSymbols:
    override val scopeSymbols: ScopeSymbols = ss
