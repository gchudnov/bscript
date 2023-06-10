package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.builder.pass.Pass

/**
 * (2-PASS)
 *
 * Executed **after** ScopeBuild Pass that created scopes and defined symbols in these scopes.
 *
 * ScopeResolve Pass:
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
    val folder = Folder.make()

    val state0         = PassState.from(in)
    val state1         = folder.foldAST(state0, in.ast)

    val out = PassState.into(state1, in.ast, in.scopeAsts)

    out
