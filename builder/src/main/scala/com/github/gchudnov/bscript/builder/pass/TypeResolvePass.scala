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
import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * #3 - Type Resolve Pass
 *
 *   - Resolve types of the AST nodes.
 */
final class TypeResolvePass extends Pass[HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST, HasEvalTypes]:

  override def run(in: HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST): HasEvalTypes =
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

      case x: Id =>
        resolveIdType(s, x) // TODO: DONE, remove this comment

      case x: BuiltInDecl =>
        foldOverAST(s, x).assignVoid(x) // TODO: DONE, remove this comment
      case x: MethodDecl =>
        foldOverAST(s, x).assignVoid(x) // TODO: DONE, remove this comment
      case x: StructDecl =>
        foldOverAST(s, x).assignVoid(x) // TODO: DONE, remove this comment
      case x: VarDecl =>
        resolveVarDeclType(s, x) // TODO: DONE, remove this comment
      case x: TypeDecl =>
        foldOverAST(s, x).assignVoid(x) // TODO: DONE, remove this comment

      case x: Annotated =>
        foldOverAST(s, x)

      case x: Assign =>
        foldOverAST(s, x).assignVoid(x) // TODO: DONE, remove this comment

      case x: Block =>
        resolveBlockType(s, x) // TODO: DONE, remove this comment

      case x @ Call(id, args) =>
        foldOverAST(s, x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)

      case x: If =>
        resolveIfType(s, x) // TODO: DONE, remove this comment

      case x @ Pair(key, value) =>
        foldOverAST(s, x)

      case x @ ConstLit(const) =>
        s.assignType(x, Const.toTypeAST(const)) // TODO: DONE, remove this comment

      case x @ CollectionLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s, x)

      case x: TypeId =>
        resolveTypeIdType(s, x) // TODO: DONE, remove this comment

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
   * Resolve the type of VarDecl
   */
  private def resolveVarDeclType(s: TypeResolveState, v: VarDecl): TypeResolveState =
    val s1 = foldOverAST(s, v)

    val aType = v.aType match
      case Auto() =>
        v.expr match
          case Init() =>
            throw BuilderException(s"Type of the variable '${v.name}' is not defined; Auto() = Init() is not supported")
          case other =>
            s1.typeOf(other)
      case other =>
        s1.typeOf(other)
    val s2 = s1.assignType(v.aType, aType)

    // handle Init(), take the value from the type of the variable
    val s3 =
      v.expr match
        case Init() =>
          s2.assignType(v.expr, aType)
        case _ =>
          s2

    s3.assignVoid(v)

  /**
   * Resolve type of the block
   */
  private def resolveBlockType(s: TypeResolveState, b: Block): TypeResolveState =
    val s1 = foldOverAST(s, b)
    val s2 = b.exprs.lastOption.fold(s1)(lastExpr => s1.assignType(b, s1.typeOf(lastExpr)))
    s2

  /**
   * Resolve type of the if expression
   *
   * if (cond) then1 else else1
   */
  private def resolveIfType(s: TypeResolveState, i: If): TypeResolveState =
    val s1       = foldOverAST(s, i)
    val thenType = s1.typeOf(i.then1)
    val elseType = s1.typeOf(i.else1)

    val s2 =
      if thenType == elseType then s1.assignType(i, thenType)
      else if TypeAST.isNothing(thenType) then s1.assignType(i, elseType)
      else if TypeAST.isNothing(elseType) then s1.assignType(i, thenType)
      else throw BuilderException(s"Type mismatch in if expression: ${thenType} and ${elseType}")
    s2

  /**
   * Resolve Id type
   */
  private def resolveIdType(s: TypeResolveState, id: Id): TypeResolveState =
    val typeAST = s.resolveIdType(id)
    val s1      = s.assignType(id, typeAST)
    s1

  /**
   * Resolve TypeId type
   */
  private def resolveTypeIdType(s: TypeResolveState, typeId: TypeId): TypeResolveState =
    val typeAST = s.resolveTypeIdType(typeId)
    val s1      = s.assignType(typeId, typeAST)
    s1

/**
 * Type Resolve State
 */
private final case class TypeResolveState(scopeTree: ReadScopeTree, scopeSymbols: ReadScopeSymbols, scopeAsts: ReadScopeAsts, evalTypes: EvalTypes):
  import TypeResolveState.*

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
  def assignType(ast: AST, t: TypeAST): TypeResolveState =
    copy(evalTypes = evalTypes.assignType(ast, t))

  /**
   * Assign void type to the AST node.
   *
   * @param ast
   *   AST node
   * @return
   *   new state
   */
  def assignVoid(ast: AST): TypeResolveState =
    assignType(ast, BuiltInType(TypeName.void))

  /**
   * Get type of the AST node.
   *
   * @param ast
   *   AST node
   * @return
   *   type
   */
  def typeOf(ast: AST): TypeAST =
    val ot = evalTypes.typeOf(ast)
    ot.getOrElse(throw BuilderException(s"Type of the AST node is not defined: ${ast}, this is a bug."))

  /**
   * Resolve Id
   *
   * @param id
   *   Id
   * @return
   *   the list of scope-decl paris
   */
  private def resolveIdDecl(id: Id): Either[Throwable, List[ScopeDecl]] =
    resolveRefDecl(id)

  /**
   * Resolve TypeId
   *
   * @param typeId
   *   type id
   * @return
   *   An error or the list of scope declarations
   */
  private def resolveTypeIdDecl(typeId: TypeId): Either[Throwable, List[ScopeDecl]] =
    resolveRefDecl(typeId)

  /**
   * Resolve T Declarations
   *
   * @param name
   *   name of the declaration to resolve
   * @return
   *   An error or the list of scope declarations
   */
  private def resolveRefDecl[R <: AST: HasName](ref: R): Either[Throwable, List[ScopeDecl]] =
    for
      startScope   <- scopeAsts.scope(ref).toRight(BuilderException(s"AST '${ref}' is not assigned to a Scope, it is a bug"))
      maybeScopeSym = scopeSymbols.resolveUp(summon[HasName[R]].name(ref), startScope, scopeTree)
      scopeDecls = maybeScopeSym.fold(List.empty[ScopeDecl]) { scopeSym =>
                     scopeAsts
                       .findDecl(scopeSym.symbol.name, scopeSym.scope)
                       .map(d => ScopeDecl(scopeSym.scope, d))
                   }
    yield scopeDecls

  /**
   * Resolve type of the Id
   *
   * // TODO: we assume that the type is already resolved, fix this
   *
   * @param id
   *   Id
   * @return
   *   type
   */
  def resolveIdType(id: Id): TypeAST =
    val scopeDecls = resolveIdDecl(id).toTry.get
    val scopeDecl  = scopeDecls.headOption.getOrElse(throw BuilderException(s"The symbol '${id}' is not declared"))
    val aType = scopeDecl.decl.aType match
      case x: TypeId =>
        resolveTypeIdType(x)
      case other =>
        typeOf(other)
    aType

  /**
   * Resolve type of the TypeId
   *
   * // TODO: we assume that the type is already resolved, fix this
   *
   * @param typeId
   *   type id
   * @return
   *   type
   */
  def resolveTypeIdType(typeId: TypeId): TypeAST =
    val scopeDecls = resolveTypeIdDecl(typeId).toTry.get
    val scopeDecl  = scopeDecls.headOption.getOrElse(throw BuilderException(s"The type-symbol '${typeId}' is not declared"))
    val aType      = scopeDecl.decl.aType
    aType

/**
 * Type Resolve State Companion
 */
private object TypeResolveState:

  trait HasName[T]:
    def name(t: T): String

  object HasName:
    inline given HasName[Id] with
      def name(id: Id): String = id.name

    inline given HasName[TypeId] with
      def name(typeId: TypeId): String = typeId.name

  def from(scopeTree: ReadScopeTree, scopeSymbols: ReadScopeSymbols, scopeAsts: ReadScopeAsts): TypeResolveState =
    TypeResolveState(
      scopeTree = scopeTree,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts,
      evalTypes = EvalTypes.empty,
    )
