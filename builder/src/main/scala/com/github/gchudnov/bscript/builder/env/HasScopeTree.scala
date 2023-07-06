package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeTree
import com.github.gchudnov.bscript.builder.state.ReadScopeTree
import com.github.gchudnov.bscript.builder.state.WriteScopeTree

trait HasReadScopeTree:
  def scopeTree: ReadScopeTree

trait HasWriteScopeTree:
  def scopeTree: WriteScopeTree

trait HasScopeTree:
  def scopeTree: ScopeTree

object HasScopeTree:
  def apply(st: ScopeTree): HasScopeTree = new HasScopeTree:
    override val scopeTree: ScopeTree = st
