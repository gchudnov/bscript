package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol

final case class OutState(
  ast: AST,
  forest: Forest[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts
)

object OutState:

  extension (s: OutState)
    def forestSize: Int =
      s.forest.size

    def scopeByAST(ast: AST): Option[Scope] =
      s.scopeAsts.scope(ast)

    /**
     * Find all symbols that have the given name
     */
    def symbolsByName(name: String): List[Symbol] =
      s.scopeSymbols.symbolsByName(name)

    /**
     * Find all scopes that contain symbols with the given name
     * 
     * TODO: ordering???
     */
    def scopesBySymbol(sym: Symbol): List[Scope] =
      s.scopeSymbols
        .symbolsByName(sym.name)
        .flatMap(it => s.scopeSymbols.scope(it).map(List(_)).getOrElse(List.empty[Scope]))
