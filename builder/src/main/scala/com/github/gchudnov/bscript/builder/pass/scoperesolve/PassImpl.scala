package com.github.gchudnov.bscript.builder.pass.scoperesolve

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
import com.github.gchudnov.bscript.builder.pass.scoperesolve.InState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.OutState

/**
 * (2-PASS)
 *
 * Executed **after** ScopeBuildFolder that created scopes and defined symbols in these scopes.
 *
 * Folder:
 *
 * {{{
 * 3) Resolve Symbols (and verify that names can be referenced).
 * }}}
 *
 * All we have to do is a depth-first walk of the AST, executing actions in the pre- and/or post-order position. When we see a symbol, we resolve it in the current scope.
 */
private[builder] final class PassImpl() extends Pass:

  type In = InState

  type Out = OutState

  override def run(in: InState): OutState =
    // val folder = ScopeResolveFolder.make()
    // val state = ScopeResolveState.make()

    // val ast = ???

    // folder.foldAST(state, ast)

    ???

// TODO: WE NEED TO DEFINE AN API TO GO FROM ONE PASS TO THE NEXT ONE
