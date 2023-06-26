package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.func.AstFolder
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.interfaces.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.util.TreeCursor

/**
 * #1 - Scope Build Pass
 *
 *   - Construct a scope tree.
 *   - Create and populate symbol tables for each scope.
 *
 * A symbol table contains a record of all the names that are declared for a scope.
 */
final class ScopeBuildPass extends Pass[HasAST, HasScopeTree & HasScopeSymbols & HasScopeAsts]:

  override def run(in: HasAST): HasScopeTree & HasScopeSymbols & HasScopeAsts =
    val state0 = ScopeBuildState.empty
    val ast0   = in.ast

    val folder = new ScopeBuildFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = new HasScopeTree with HasScopeSymbols with HasScopeAsts:
      override val scopeTree: ScopeTree       = state1.scopeCursor.tree
      override val scopeSymbols: ScopeSymbols = state1.scopeSymbols
      override val scopeAsts: ScopeAsts       = state1.scopeAsts

    out

/**
 * Scope Build Folder
 */
private final class ScopeBuildFolder() extends AstFolder[ScopeBuildState]:

  override def foldAST(s: ScopeBuildState, ast: AST): ScopeBuildState =
    ast match
      case x: Access =>
        foldOverAST(s, x)
      case x @ Id(name) =>
        foldOverAST(s, x)

      // TODO: we want to add name, not only fullName to a symbol

      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s.defineSymbol(SMethod(x.fullName)).bindAstToScope(x).pushScope(), x).popScope()
      case x @ StructDecl(name, sType) =>
        foldOverAST(s.defineSymbol(SStruct(x.fullName)).bindAstToScope(x).pushScope(), x).popScope()
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s.defineSymbol(SVar(x.fullName)).bindAstToScope(x), x)
      case x @ TypeDecl(name) =>
        foldOverAST(s.defineSymbol(SType(x.fullName)).bindAstToScope(x), x)

      case x: Annotated =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s.pushScope(), x).popScope()
      case x @ Call(id, args) =>
        foldOverAST(s.bindAstToScope(x), x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)
      case x: If =>
        foldOverAST(s, x)
      case x @ Init() =>
        foldOverAST(s, x)
      case x @ KeyValue(key, value) =>
        foldOverAST(s, x)

      case x @ ConstLit(const) =>
        foldOverAST(s, x)
      case x @ CollectionLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s.pushScope(), x).popScope()

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        foldOverAST(s, x).bindAstToScope(x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)

      case other =>
        throw new MatchError(s"Unsupported AST type in ScopeBuildFolder: ${other}")

/**
 * Scope Build State
 */
private final case class ScopeBuildState(scopeCursor: TreeCursor[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts):

  def pushScope(): ScopeBuildState =
    this.copy(scopeCursor = scopeCursor.push())

  def popScope(): ScopeBuildState =
    this.copy(scopeCursor = scopeCursor.pop())

  def defineSymbol(symbol: Symbol): ScopeBuildState =
    scopeCursor.at match
      case Some(scope) =>
        if scopeSymbols.hasLink(scope, symbol) then throw new BuilderException(s"Symbol '${symbol}' is already defined in the '${scope}' scope")
        else this.copy(scopeSymbols = scopeSymbols.link(scope, symbol))
      case None =>
        throw new BuilderException(s"Cannot define symbol '${symbol}' without a current scope. Call .pushScope() to create a scope first")

  def bindAstToScope(ast: AST): ScopeBuildState =
    scopeCursor.at match
      case Some(scope) =>
        if scopeAsts.hasLink(scope, ast) then throw new BuilderException(s"AST '${ast}' is already bound to the '${scope}' scope")
        else this.copy(scopeAsts = scopeAsts.link(scope, ast))
      case None =>
        throw new BuilderException(s"Cannot bind AST '${ast}' to a scope without a current scope. Invoke .pushScope() to create a scope first")

  def scopeTree: ScopeTree =
    scopeCursor.tree

/**
 * Scope Build State Companion
 */
private object ScopeBuildState:

  lazy val empty: ScopeBuildState =
    ScopeBuildState(
      scopeCursor = TreeCursor.empty[Scope](it => ScopeRef(it)),
      scopeSymbols = ScopeSymbols.empty,
      scopeAsts = ScopeAsts.empty,
    )
