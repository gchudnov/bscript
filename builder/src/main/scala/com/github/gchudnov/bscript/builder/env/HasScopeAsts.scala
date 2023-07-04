package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeAsts

trait HasScopeAsts:
  def scopeAsts: ScopeAsts

object HasScopeAsts:
  def apply(ss: ScopeAsts): HasScopeAsts = new HasScopeAsts:
    override val scopeAsts: ScopeAsts = ss
