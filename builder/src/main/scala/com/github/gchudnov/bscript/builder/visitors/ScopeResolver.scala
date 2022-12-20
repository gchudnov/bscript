package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.visitors.internal.BasicScopeResolver
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.builder.ScopeRef

/**
  * ScopeResolver
  */
trait ScopeResolver:
  def defineVar(scope: ScopeRef, name: String, vType: TypeRef): ScopeResolver

  def result: Meta

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(scopeAsts: ScopeAsts): ScopeResolver =
    new BasicScopeResolver(scopeAsts)
