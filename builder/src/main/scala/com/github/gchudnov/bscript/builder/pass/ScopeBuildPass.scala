package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.func.ASTFolder
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.util.TreeCursor
import com.github.gchudnov.bscript.lang.ast.refs.*

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
private final class ScopeBuildFolder() extends ASTFolder[ScopeBuildState]:

  override def foldAST(s: ScopeBuildState, ast: AST): ScopeBuildState =
    ast match
      case x @ Id(name) =>
        foldOverAST(s.bindAstToScope(x), x)

      case x @ BuiltInDecl(name, tType) =>
        foldOverAST(s.defineSymbol(SType(x.name)).bindAstToScope(x), x)
      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s.defineSymbol(SMethod(x.name, mType.signature)).bindAstToScope(x).pushScope(), x).popScope()
      case x @ StructDecl(name, sType) =>
        foldOverAST(s.defineSymbol(SStruct(x.name)).bindAstToScope(x).pushScope(), x).popScope()
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s.defineSymbol(SVar(x.name)).bindAstToScope(x), x)
      case x @ TypeDecl(name, tType) =>
        foldOverAST(s.defineSymbol(SType(x.name)).bindAstToScope(x), x)

      case x: Block =>
        foldOverAST(s.pushScope(), x).popScope()
      case x @ Call(id, args) =>
        foldOverAST(s.bindAstToScope(x), x)

      case x @ MethodLit(mType, body) =>
        foldOverAST(s.pushScope(), x).popScope()

      case x @ TypeId(name) =>
        foldOverAST(s, x).bindAstToScope(x)

      case other =>
        foldOverAST(s, other)

/**
 * Scope Build State
 */
private final case class ScopeBuildState(scopeCursor: TreeCursor[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts):

  /**
   * Push a new scope
   *
   * @return
   *   a new state
   */
  def pushScope(): ScopeBuildState =
    this.copy(scopeCursor = scopeCursor.push())

  /**
   * Pop the current scope
   *
   * @return
   *   a new state
   */
  def popScope(): ScopeBuildState =
    this.copy(scopeCursor = scopeCursor.pop())

  /**
   * Define a Symbol in the current scope.
   *
   * @param symbol
   *   the symbol to define
   * @return
   *   a new state
   */
  def defineSymbol(symbol: Symbol): ScopeBuildState =
    val errOrState =
      for ss <- defineSymbolInCurrentScope(symbol)
      yield this.copy(scopeSymbols = ss)
    errOrState.fold(throw _, identity)

  /**
   * Bind an AST to the current scope.
   *
   * @param ast
   *   the AST to bind
   * @return
   *   a new state
   */
  def bindAstToScope(ast: AST): ScopeBuildState =
    val errOrState =
      for sa <- bindAstToCurrentScope(ast)
      yield this.copy(scopeAsts = sa)
    errOrState.fold(throw _, identity)

  /**
   * Return the current scope tree.
   *
   * @return
   *   the current scope tree
   */
  def scopeTree: ScopeTree =
    scopeCursor.tree

  /**
   * Define a sybol in the current scope.
   *
   * @param symbol
   *   symbol to define
   * @return
   *   An error of the new scope symbols
   */
  private def defineSymbolInCurrentScope(symbol: Symbol): Either[Throwable, ScopeSymbols] =
    for
      scope <- scopeCursor.at.toRight(new BuilderException(s"Cannot define symbol '${symbol}' without a current scope. Call .pushScope() to create a scope first"))
      _     <- if scopeSymbols.hasLink(scope, symbol) then Left(new BuilderException(s"Symbol '${symbol}' is already defined in the '${scope}' scope")) else Right(())
      ss     = scopeSymbols.link(scope, symbol)
    yield ss

  /**
   * Bind an AST to the current scope.
   *
   * @param ast
   *   the AST to bind
   * @return
   *   An error of the new scope ASTs
   */
  private def bindAstToCurrentScope(ast: AST): Either[Throwable, ScopeAsts] =
    for
      scope <- scopeCursor.at.toRight(new BuilderException(s"Cannot bind AST '${ast}' to a scope without a current scope. Invoke .pushScope() to create a scope first"))
      _     <- if scopeAsts.hasLink(scope, ast) then Left(new BuilderException(s"AST '${ast}' is already bound to the '${scope}' scope")) else Right(())
      sa     = scopeAsts.link(scope, ast)
    yield sa

/**
 * Scope Build State Companionsymbol: Symbol
 */
private object ScopeBuildState:

  lazy val empty: ScopeBuildState =
    ScopeBuildState(
      scopeCursor = TreeCursor.empty[Scope](it => ScopeRef(it)),
      scopeSymbols = ScopeSymbols.empty,
      scopeAsts = ScopeAsts.empty,
    )
