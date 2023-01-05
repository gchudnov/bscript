package com.github.gchudnov.bscript.builder.internal.scopebuilder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.func.AstFolder

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
 * ScopeBuildFolder:
 *
 * {{{
 * 1) Pushes Scopes;
 * 2) Defines symbols in scopes;
 * 3) Pops Scopes;
 * }}}
 *
 * For (2-PASS) -- see ScopeResolveFolder
 *
 * Building a scope tree boils down to executing a sequence of these operations: push, pop, and def.
 *
 * [push]. At the start of a scope, push a new scope on the scope stack. This works even for complicated scopes like classes. Because we are building scope trees, push is more like
 * an “add child” tree construction operation than a conventional stack push. An implementation preview:
 *
 * {{{
 *   // create new scope whose enclosing scope is the current scope
 *   val currentScope = new LocalScope(currentScope); // push new scope
 * }}}
 *
 * [pop]. At the end of a scope, pop the current scope off the stack, revealing the previous scope as the current scope. pop moves the current scope pointer up one level in the
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
 */
private[scopebuilder] final class ScopeBuildFolder() extends AstFolder[ScopeBuildState]:

  override def foldAST(s: ScopeBuildState, ast: AST): ScopeBuildState =
    ast match
      case x: Access =>
        foldOverAST(s, x)
      case x @ Id(name) =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s.push(), x).pop()
      case x @ Literal(const) =>
        foldOverAST(s, x)
      case x @ Call(id, args) =>
        foldOverAST(s.bind(x), x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)
      case x: If =>
        foldOverAST(s, x)
      case x @ Init() =>
        foldOverAST(s, x)
      case x @ MethodDecl(name, tparams, params, retType, body) =>
        foldOverAST(s.define(SMethod(name)).push(), x).pop()
      case x @ StructDecl(name, tfields, fields) =>
        foldOverAST(s.define(SStruct(name)).push(), x).pop()
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s.define(SVar(name)).bind(x), x)
      case x @ TypeDecl(name) =>
        foldOverAST(s.define(SVar(name)).bind(x), x)
      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(_) =>
        foldOverAST(s, x)
      case x @ Applied(aType, args) =>
        foldOverAST(s, x)

private[scopebuilder] object ScopeBuildFolder:

  def make(): ScopeBuildFolder =
    new ScopeBuildFolder()
