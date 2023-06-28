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

/**
 * #2 - Variable Resolve Pass
 *
 *   - Checking for undeclared variables.
 *
 * To find undeclared variables, check the symbol table on each variable that's used for assignment or dereferencing.
 */
final class VarResolvePass extends Pass[HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST, Unit]:

  override def run(in: HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST): Unit =
    val state0 = VarResolveState.from(in.scopeTree, in.scopeSymbols, in.scopeAsts)
    val ast0   = in.ast

    val folder = new VarResolveFolder()

    val state1 = folder.foldAST(state0, ast0)

    // TODO: given that we're not changing the state, we do not need to return anything?

    val out = ()

    out

// TODO: after resolving the variable, most likely we need to store it so that we can use it later

/**
 * Variable Resolve Folder
 */
private final class VarResolveFolder() extends AstFolder[VarResolveState]:

  override def foldAST(s: VarResolveState, ast: AST): VarResolveState =
    ast match
      case x: Access =>
        foldOverAST(s.resolveAccess(x), x)
      case x @ Id(name) =>
        foldOverAST(s.resolveId(x), x)

      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s, x)
      case x @ StructDecl(name, sType) =>
        foldOverAST(s, x)
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s, x)
      case x @ TypeDecl(name) =>
        foldOverAST(s, x)

      case x: Annotated =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s, x)
      case x @ Call(id, args) =>
        // TODO: here
        foldOverAST(s, x)
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
        foldOverAST(s, x)

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        // TODO: here?
        foldOverAST(s, x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)

      case other =>
        throw new MatchError(s"Unsupported AST type in VarResolveFolder: ${other}")

/**
 * Var Resolve State
 */
private final case class VarResolveState(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts):

  /**
   * Resolve Id
   *
   * @param id
   *   id
   * @return
   *   an updated state
   */
  def resolveId(id: Id): VarResolveState =
    val errOrState = for
      startScope <- scopeAsts.scope(id).toRight(BuilderException(s"AST '${id}' is not assigned to a Scope, it is a bug"))
      _       <- scopeSymbols.resolveUp(id.name, startScope, scopeTree).toRight(BuilderException(s"Symbol '${id.name}' is not found in the scope tree"))
    yield this
    errOrState.fold(throw _, identity)

  /**
    * Resolve Access
    *
    * @param access
    *   access
    * @return
    *   an updated state
    */
  def resolveAccess(access: Access): VarResolveState =
    ???

  // TODO: impl it

  // TODO: given that Sybmol is not unique, we need to create SymbolPtr that consists of Symbol and Scope
  


/**7
 * Scope Build State Companion
 */
private object VarResolveState:

  def from(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): VarResolveState =
    VarResolveState(
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
    )
