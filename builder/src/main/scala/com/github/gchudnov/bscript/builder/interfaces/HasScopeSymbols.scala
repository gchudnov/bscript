package com.github.gchudnov.bscript.builder.interfaces

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeSymbols

trait HasScopeSymbols:
  def scopeSymbols: ScopeSymbols

object HasScopeSymbols:
  def apply(ss: ScopeSymbols): HasScopeSymbols = new HasScopeSymbols:
    override def scopeSymbols: ScopeSymbols = ss
