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
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id
import com.github.gchudnov.bscript.lang.const.Const

/**
 * #3 - Type Resolve Pass
 *
 *   - Resolve types of the AST nodes.
 *
 */
final class TypeResolvePass extends Pass[HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST, HasEvalTypes]:

  override def run(in: HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST): HasEvalTypes =
    val state0 = TypeResolveState.from(in.scopeTree, in.scopeSymbols, in.scopeAsts)
    val ast0   = in.ast

    val folder = new TypeResolveFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = new HasEvalTypes:
      override val evalTypes: EvalTypes = state1.evalTypes

    out

/**
 * Type Resolve Folder
 */
private final class TypeResolveFolder() extends ASTFolder[TypeResolveState]:

  override def foldAST(s: TypeResolveState, ast: AST): TypeResolveState =
    ast match
      case x: Access =>
        foldOverAST(s, x)

      case x @ Id(name) =>
        foldOverAST(s, x)

//   override def visit(s: PassState, n: Var): Either[Throwable, PassState] =
//     for
//       scope <- s.meta.scopeFor(n)
//       sVar  <- s.meta.resolve(n.symbol.name, scope).flatMap(_.asSVar)
//       n1     = n.copy(symbol = sVar)
//       ss1    = s.meta.redefineASTScope(n, n

      case x @ BuiltInDecl(name, tType) =>
        foldOverAST(s, x)
      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s, x)
      case x @ StructDecl(name, sType) =>
        foldOverAST(s, x)
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s, x)
      case x @ TypeDecl(name, tType) =>
        foldOverAST(s, x)

      case x: Annotated =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s, x)
      case x @ Call(id, args) =>
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
        s.assign(x, Const.toTypeAST(const)) // TODO: impl complete

      case x @ CollectionLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s, x)

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        foldOverAST(s, x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)
      case x @ GenericType(name) =>
        foldOverAST(s, x)

      case other =>
        foldOverAST(s, other)

      // case other =>
      //   throw new MatchError(s"Unsupported AST type in TypeResolveFolder: ${other}")

/**
 * Type Resolve State
 */
private final case class TypeResolveState(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts, evalTypes: EvalTypes) {
  /**
    * Assign type to the AST node.
    *
    * @param ast
    *   AST node
    * @param t
    *   type
    * @return
    *   new state
    */
  def assign(ast: AST, t: TypeAST): TypeResolveState =
    copy(evalTypes = evalTypes.assign(ast, t))
}

/**
 * Type Resolve State Companion
 */
private object TypeResolveState:

  def from(scopeTree: ScopeTree, scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts): TypeResolveState =
    TypeResolveState(
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
      evalTypes = EvalTypes.empty,
    )
