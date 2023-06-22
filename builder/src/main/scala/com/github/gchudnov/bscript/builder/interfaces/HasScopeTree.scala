package com.github.gchudnov.bscript.builder.interfaces

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.ScopeTree

trait HasScopeTree:
  def scopeTree: ScopeTree

object HasScopeTree:
  def apply(st: ScopeTree): HasScopeTree = new HasScopeTree:
    override def scopeTree: ScopeTree = st
