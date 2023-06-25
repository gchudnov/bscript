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
 * #2 - Variable Resolve Pass
 *
 *   - Checking for undeclared variables.
 *
 * To find undeclared variables, check the symbol table on each variable that's used for assignment or dereferencing.
 */
final class VarResolvePass extends Pass[HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST, HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST]:

  override def run(in: HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST): HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST =
    val state0 = VarResolveState.from(in.scopeTree, in.scopeSymbols, in.scopeAsts)
    val ast0   = in.ast

    val folder = new VarResolveFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val ast: AST                   = ast0
      override val scopeTree: ScopeTree       = in.scopeTree
      override val scopeSymbols: ScopeSymbols = in.scopeSymbols
      override val scopeAsts: ScopeAsts       = state1.scopeAsts

    out

/**
 * Variable Resolve Folder
 */
private final class VarResolveFolder() extends AstFolder[VarResolveState]:

    //   case x @ Id(name) =>
    //     val symbol = scopeSymbols.get(name)
    //     if symbol.isEmpty then
    //       throw BuilderException(s"Undefined variable: '$name'", x.pos)
    //     else
    //       s.bindAstToScope(x)

  override def foldAST(s: VarResolveState, ast: AST): VarResolveState =
    ast match
      case x: Access =>
        // TODO: here
        foldOverAST(s, x)
      case x @ Id(name) =>
        // TODO: here
        foldOverAST(s, x)

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
private final case class VarResolveState(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts) {

}

/**
 * Scope Build State Companion
 */
private object VarResolveState:

  def from(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): VarResolveState =
    VarResolveState(
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
    )
