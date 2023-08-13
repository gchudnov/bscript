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
import com.github.gchudnov.bscript.lang.ast.refs.*

/**
 * #2 - Symbol Resolve Pass
 *
 *   - Checking for undeclared symbols (variables, types, type parameters).
 *
 * To find undeclared symbols, check the symbol table on each variable that's used for assignment or dereferencing.
 */
final class SymbolResolvePass extends Pass[HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST, Unit]:

  override def run(in: HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST): Unit =
    val state0 = SymbolResolveState.from(in.scopeTree, in.scopeSymbols, in.scopeAsts)
    val ast0   = in.ast

    val folder = new SymbolResolveFolder()

    val state1 = folder.foldAST(state0, ast0)

    // TODO: given that we're sesolving pointers here, store so that we can use them later without resolving again

    // TODO: should we allow cycles? A->B->C->A ???

    // TODO: check if we want to replace ScopeSymbol with ScopeDecl ???

    val out = ()

    out

/**
 * Symbol Resolve Folder
 */
private final class SymbolResolveFolder() extends ASTFolder[SymbolResolveState]:

  override def foldAST(s: SymbolResolveState, ast: AST): SymbolResolveState =
    ast match
      case x: Access =>
        s.ensureAccess(x)
      case x: Id =>
        s.ensureId(x)
      case x: TypeId =>
        s.ensureTypeId(x)

      case other =>
        foldOverAST(s, other)

/**
 * Symbol Resolve State
 */
private final case class SymbolResolveState(scopeTree: ReadScopeTree, scopeSymbols: ReadScopeSymbols, scopeAsts: ReadScopeAsts):

  /**
   * Ensure that TypeId is resolved
   *
   * @param typeId
   *   type id
   * @return
   *   an updated state
   */
  def ensureTypeId(typeId: TypeId): SymbolResolveState =
    val errOrState = for
      scopeDecls <- resolveTypeId(typeId)
      _          <- scopeDecls.headOption.toRight(BuilderException(s"TypeId '${typeId.name}' is not found in the scope tree"))
    yield this
    errOrState.fold(throw _, identity)

  /**
   * Ensure that Id is resolved
   *
   * @param id
   *   id
   * @return
   *   an updated state
   */
  def ensureId(id: Id): SymbolResolveState =
    val errOrState = for
      maybeScopeSymbol <- resolveId(id)
      _                <- maybeScopeSymbol.toRight(BuilderException(s"Id '${id.name}' is not found in the scope tree"))
    yield this
    errOrState.fold(throw _, identity)

  /**
   * Ensure that Access is resolved
   *
   * @param access
   *   access
   * @return
   *   an updated state
   */
  def ensureAccess(access: Access): SymbolResolveState =
    val errOrState = for
      maybeScopeSymbol <- resolveAccess(access)
      _                <- maybeScopeSymbol.toRight(BuilderException(s"Acccess '${access.path.mkString(".")}' is not found in the scope tree"))
    yield this
    errOrState.fold(throw _, identity)

  /**
   * Resolve Id
   *
   * @param id
   *   id
   * @return
   *   an updated state
   */
  private def resolveId(id: Id): Either[Throwable, Option[ScopeSymbol]] =
    for
      startScope      <- scopeAsts.scope(id).toRight(BuilderException(s"AST '${id}' is not assigned to a Scope, it is a bug"))
      maybeScopeSymbol = scopeSymbols.resolveUp(id.name, startScope, scopeTree)
    yield maybeScopeSymbol

  /**
   * Resolve Access
   *
   * At the moment we resolve only top-level access, given that Auto and Generic types are not specified yet.
   *
   * {{{
   *   x.y    // we resolve only `x`
   *   x.y.z  // we resolve only `x`
   * }}}
   *
   * @param access
   *   access
   * @return
   *   an updated state
   */
  private def resolveAccess(access: Access): Either[Throwable, Option[ScopeSymbol]] =
    (access.a, access.b) match
      case (x: Id, _: Id) =>
        resolveId(x)
      case (x: Access, _: Id) =>
        resolveAccess(x)

  /**
   * Resolve TypeId
   * 
   * NOTE: it must point to a TypeDecl
   *
   * @param typeId
   *   type id
   * @return
   *   An error or the list of scope declarations
   */
  private def resolveTypeId(typeId: TypeId): Either[Throwable, List[ScopeDecl]] =
    for
      startScope   <- scopeAsts.scope(typeId).toRight(BuilderException(s"AST '${typeId}' is not assigned to a Scope, it is a bug"))
      maybeScopeSym = scopeSymbols.resolveUp(typeId.name, startScope, scopeTree)
      scopeDecls = maybeScopeSym.fold(List.empty[ScopeDecl]) { scopeSym =>
                     scopeAsts
                       .findDecl(scopeSym.symbol.name, scopeSym.scope)
                       .map(d => ScopeDecl(scopeSym.scope, d))
                   }
    yield scopeDecls

/**
 * Scope Build State Companion
 */
private object SymbolResolveState:

  def from(scopeTree: ReadScopeTree, scopeSymbols: ReadScopeSymbols, scopeAsts: ReadScopeAsts): SymbolResolveState =
    SymbolResolveState(
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
    )
