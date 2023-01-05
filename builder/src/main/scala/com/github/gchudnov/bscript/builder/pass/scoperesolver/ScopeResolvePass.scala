package com.github.gchudnov.bscript.builder.pass.scoperesolver

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.internal.scoperesolver.BasicScopeResolver
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.types.TypeRef
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.state.VarTypes
import com.github.gchudnov.bscript.lang.ast.types.TypeAST

/**
  * ScopeResolver
  */
trait ScopeResolver:

  def resolveVarDecl(name: String, vType: TypeAST, ast: AST): ScopeResolver

  def result: Meta

/**
 * ScopeResolver
 */
object ScopeResolver:
  def make(forest: Forest[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): ScopeResolver =
    new BasicScopeResolver(forest, scopeSymbols, scopeAsts, VarTypes.empty)
