package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeTree

trait HasScopeTree:
  def scopeTree: ScopeTree

object HasScopeTree:
  def apply(st: ScopeTree): HasScopeTree = new HasScopeTree:
    override val scopeTree: ScopeTree = st
