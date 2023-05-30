package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.pass.Pass
import com.github.gchudnov.bscript.builder.pass.scopebuild.Folder
import com.github.gchudnov.bscript.builder.pass.scopebuild.InState
import com.github.gchudnov.bscript.builder.pass.scopebuild.OutState
import com.github.gchudnov.bscript.builder.pass.scopebuild.PassState
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol

/**
 * (1-PASS)
 *
 * The primary goal when building a symbol table is to construct a scope tree.
 *
 * We define symbols, group them into scopes, and organize those scopes into scope trees.
 *
 * Scope trees are crucial because their structure encodes the rules for looking up symbols.
 *
 * Resolving a symbol means looking for it in the current scope or any scope on the path to the root of the scope tree.
 *
 * At this pass, symbols should be defined in scopes.
 *
 * ScopeBuildPass:
 *
 * {{{
 * 1) Pushes Scopes;
 * 2) Defines symbols in scopes;
 * 3) Pops Scopes;
 * }}}
 *
 * Building a scope tree boils down to executing a sequence of these operations: *push*, *pop*, and *def*.
 *
 * [push]. At the start of a scope, `push` a new scope on the scope stack. This works even for complicated scopes like classes. Because we are building scope trees, push is more
 * like an “add child” tree construction operation than a conventional stack `push`. An implementation preview:
 *
 * {{{
 *   // create new scope whose enclosing scope is the current scope
 *   val currentScope = new LocalScope(currentScope); // push new scope
 * }}}
 *
 * [pop]. At the end of a scope, *pop* the current scope off the stack, revealing the previous scope as the current scope. pop moves the current scope pointer up one level in the
 * tree:
 *
 * {{{
 *   val currentScope = currentScope.getEnclosingScope(); // pop scope
 * }}}
 *
 * [def]. Define a symbol in the current scope. We’ll always define symbols like this:
 *
 * {{{
 *   val s: Symbol = «some-new-symbol»;
 *   currentScope.define(s); // define s in current scope
 * }}}
 *
 * To create a scope tree then, all we have to do is a depth-first walk of the AST, executing actions in the pre- and/or post-order position. We push as we descend and pop as we
 * ascend. When we see a symbol, we define or resolve it in the current scope.
 * 
 * NOTE: for (2-PASS) -- see scoperesolve/PassImpl.scala
 */
private[builder] final class PassImpl() extends Pass:

  type In  = InState
  type Out = OutState

  override def run(in: InState): OutState =
    val folder = Folder.make()

    val state0         = PassState.from(in)
    val state1         = folder.foldAST(state0, in.ast)

    val out = PassState.into(state1, in.ast)

    out
