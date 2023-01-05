package com.github.gchudnov.bscript.builder.pass.scopebuilder

import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.pass.Pass


private[builder] final class ScopeBuildPass() extends Pass:

  override def go(): Unit =
    val folder = ScopeBuildFolder.make()
    val state = ScopeBuildState.make()

    val ast = ???

    folder.foldAST(state, ast)
