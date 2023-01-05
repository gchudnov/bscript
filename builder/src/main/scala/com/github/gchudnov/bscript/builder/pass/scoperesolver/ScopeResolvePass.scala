package com.github.gchudnov.bscript.builder.pass.scoperesolver

import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts
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

import com.github.gchudnov.bscript.builder.pass.Pass

private[builder] final class ScopeResolvePass() extends Pass:

  override def go(): Unit =
    val folder = ScopeResolveFolder.make()
    val state = ScopeResolveState.make()

    val ast = ???

    folder.foldAST(state, ast)

// TODO: WE NEED TO DEFINE AN API TO GO FROM ONE PASS TO THE NEXT ONE