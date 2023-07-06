package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ReadScopeAsts
import com.github.gchudnov.bscript.builder.state.WriteScopeAsts

trait HasReadScopeAsts:
  def scopeAsts: ReadScopeAsts

trait HasWriteScopeAsts:
  def scopeAsts: WriteScopeAsts

trait HasScopeAsts:
  def scopeAsts: ScopeAsts

object HasScopeAsts:
  def apply(ss: ScopeAsts): HasScopeAsts = new HasScopeAsts:
    override val scopeAsts: ScopeAsts = ss
