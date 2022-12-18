package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.mirror.AstFolder

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
 * ScopeBuildVisitor:
 *
 * {{{
 * 1) Pushes Scopes;
 * 2) Defines symbols in scopes;
 * 3) Pops Scopes;
 * }}}
 *
 * ScopeResolveVisitor:
 *
 * {{{
 * 3) Resolve Symbols (and verify that names can be referenced).
 * }}}
 *
 * ---
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
 * To create a scope tree then, all we have to do is a depth-first walk of the AST, executing actions in the pre- and/or postorder position. We push as we descend and pop as we
 * ascend. When we see a symbol, we define or resolve it in the current scope.
 *
 * NOTE: in the {{{ScopeBuildState]}}} it is *very* important that *different* instances of the same case class with the same value are different. It affects symbol resolution.
 */
private[builder] final class ScopeBuildVisitor() extends AstFolder[ScopeBuilder]:

  override def foldAST(a: ScopeBuilder, ast: AST): ScopeBuilder =
    ast match
      case x: Access =>
        foldOverTree(a, x)
      case x @ ArgDecl(aType, name) =>
        foldOverTree(a.define(SVar(name)), x)
      case x: Assign =>
        foldOverTree(a, x)
      case x: Block =>
        foldOverTree(a.push(), x).pop()
      case x @ Literal(_) =>
        foldOverTree(a, x)
      case x @ Call(id, args) =>
        foldOverTree(a.bind(x), x)
      case x @ Compiled(_, retType) =>
        foldOverTree(a, x)
      case x @ FieldDecl(fType, name) =>
        foldOverTree(a.define(SVar(name)), x)
      case x: If =>
        foldOverTree(a, x)
      case x @ Init(iType) =>
        foldOverTree(a, x)
      case x @ MethodDecl(retType, name, _, _) =>
        foldOverTree(a.define(SMethod(name)).push(), x).pop()
      case x @ StructDecl(name, _) =>
        foldOverTree(a.define(SStruct(name)).push(), x).pop()
      case x @ Var(sym) =>
        foldOverTree(a, x)
      case x @ VarDecl(vType, name, _) =>
        foldOverTree(a.define(SVar(name)), x)
      case x @ Vec(_, elementType) =>
        foldOverTree(a, x)

private[builder] object ScopeBuildVisitor:

  def make(): ScopeBuildVisitor =
    new ScopeBuildVisitor()
