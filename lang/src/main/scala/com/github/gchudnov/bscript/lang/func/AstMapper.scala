package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.const.*

import com.github.gchudnov.bscript.lang.ast.lit.CollectionLit
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id
import com.github.gchudnov.bscript.lang.ast.refs.Ref
import com.github.gchudnov.bscript.lang.ast.decls.BuiltInDecl

/**
 * Maps AST
 */
trait AstMapper:

  def mapAST(ast: AST): AST =
    ast match
      case a: Stat =>
        mapStat(a)
      case a: TypeAST =>
        mapTypeAST(a)
      case other =>
        throw new MatchError(s"Unsupported AST: ${other}")

  def mapStat(ast: Stat): Stat =
    ast match
      case a: Expr =>
        mapExpr(a)
      case other =>
        throw new MatchError(s"Unsupported Stat: ${other}")

  def mapTypeAST(ast: TypeAST): TypeAST =
    ast match
      case a: Auto =>
        mapAuto(a)
      case a: TypeId =>
        mapTypeId(a)
      case a: RealType =>
        mapRealType(a)
      case a: ByName =>
        mapByName(a)
      case other =>
        throw new MatchError(s"Unsupported TypeAST: ${other}")

  def mapRealType(ast: RealType): RealType =
    ast match
      case a: BuiltInType =>
        mapBuiltInType(a)
      case a: GenericType =>
        mapGenericType(a)
      case a: VecType =>
        mapVecType(a)
      case a: MapType =>
        mapMapType(a)
      case a: StructType =>
        mapStructType(a)
      case a: MethodType =>
        mapMethodType(a)
      case other =>
        throw new MatchError(s"Unsupported RealType: ${other}")

  def mapExpr(ast: Expr): Expr =
    ast match
      case a: Ref =>
        mapRef(a)
      case a: Decl =>
        mapDecl(a)
      case a: Annotated =>
        mapAnnotated(a)
      case a: Assign =>
        mapAssign(a)
      case a: Block =>
        mapBlock(a)
      case a: Call =>
        mapCall(a)
      case a: Compiled =>
        mapCompiled(a)
      case a: If =>
        mapIf(a)
      case a: Init =>
        mapInit(a)
      case a: Return =>
        mapReturn(a)
      case a: KeyValue =>
        mapKeyValue(a)
      case a: Lit =>
        mapLit(a)
      case other =>
        throw new MatchError(s"Unsupported Expr: ${other}")

  def mapRef(ast: Ref): Ref =
    ast match
      case a: Access =>
        mapAccess(a)
      case a: Id =>
        mapId(a)
      case other =>
        throw new MatchError(s"Unsupported Ref: ${other}")

  def mapDecl(ast: Decl): Decl =
    ast match
      case a: MethodDecl =>
        mapMethodDecl(a)
      case a: StructDecl =>
        mapStructDecl(a)
      case a: VarDecl =>
        mapVarDecl(a)
      case a: TypeDecl =>
        mapTypeDecl(a)
      case a: BuiltInDecl =>
        mapBuiltInDecl(a)
      case other =>
        throw new MatchError(s"Unsupported Decl: ${other}")

  def mapLit(ast: Lit): Lit =
    ast match
      case a: ConstLit =>
        mapConstLit(a)
      case a: CollectionLit =>
        mapGroupLit(a)
      case a: MethodLit =>
        mapMethodLit(a)
      case other =>
        throw new MatchError(s"Unsupported Lit: ${other}")

  def mapAccess(ast: Access): Access =
    ast.copy(a = mapRef(ast.a), b = mapId(ast.b))

  def mapId(ast: Id): Id =
    ast

  def mapAuto(ast: Auto): Auto =
    ast

  def mapTypeId(ast: TypeId): TypeId =
    ast

  def mapByName(ast: ByName): ByName =
    ast

  def mapVecType(ast: VecType): VecType =
    ast.copy(elemType = mapTypeAST(ast.elemType))

  def mapMapType(ast: MapType): MapType =
    ast.copy(keyType = ast.keyType, valueType = ast.valueType)

  def mapStructType(ast: StructType): StructType =
    ast.copy(tfields = mapTypeDecls(ast.tfields), fields = mapVarDecls(ast.fields))

  def mapMethodType(ast: MethodType): MethodType =
    ast.copy(tparams = mapTypeDecls(ast.tparams), params = mapVarDecls(ast.params), retType = mapTypeAST(ast.retType))

  def mapBuiltInType(ast: BuiltInType): BuiltInType =
    ast

  def mapGenericType(ast: GenericType): GenericType =
    ast

  def mapBuiltInDecl(ast: BuiltInDecl): BuiltInDecl =
    ast.copy(aType = mapBuiltInType(ast.aType))

  def mapTypeDecl(ast: TypeDecl): TypeDecl =
    ast.copy(aType = mapGenericType(ast.aType))

  def mapMethodDecl(ast: MethodDecl): MethodDecl =
    ast.copy(aType = mapMethodType(ast.aType), body = mapBlock(ast.body))

  def mapStructDecl(ast: StructDecl): StructDecl =
    ast.copy(aType = mapStructType(ast.aType))

  def mapVarDecl(ast: VarDecl): VarDecl =
    ast.copy(aType = mapTypeAST(ast.aType), expr = mapExpr(ast.expr))

  def mapAnnotated(ast: Annotated): Annotated =
    ast.copy(expr = mapExpr(ast.expr), id = mapRef(ast.id), tparams = mapTypeDecls(ast.tparams), params = mapExprs(ast.params))

  def mapAssign(ast: Assign): Assign =
    ast.copy(lhs = mapRef(ast.lhs), rhs = mapExpr(ast.rhs))

  def mapBlock(ast: Block): Block =
    ast.copy(exprs = mapExprs(ast.exprs))

  def mapCall(ast: Call): Call =
    ast.copy(id = mapRef(ast.id), args = mapExprs(ast.args))

  def mapCompiled(ast: Compiled): Compiled =
    ast.copy(retType = mapTypeAST(ast.retType))

  def mapIf(ast: If): If =
    ast.copy(cond = mapExpr(ast.cond), then1 = mapExpr(ast.then1), else1 = mapExpr(ast.else1))

  def mapInit(ast: Init): Init =
    ast

  def mapReturn(ast: Return): Return =
    ast.copy(expr = mapExpr(ast.expr))

  def mapKeyValue(ast: KeyValue): KeyValue =
    ast.copy(key = mapConstLit(ast.key), value = mapExpr(ast.value))

  def mapConstLit(ast: ConstLit): ConstLit =
    ast.copy(const = mapConst(ast.const))

  def mapGroupLit(ast: CollectionLit): CollectionLit =
    ast.copy(cType = mapTypeAST(ast.cType), elems = mapExprs(ast.elems))

  def mapMethodLit(ast: MethodLit): MethodLit =
    ast.copy(mType = mapMethodType(ast.mType), body = mapBlock(ast.body))

  def mapConst(const: Const): Const =
    const

  private def mapExprs(asts: List[Expr]): List[Expr] =
    asts.mapConserve(x => mapExpr(x))

  private def mapTypeDecls(tparams: List[TypeDecl]): List[TypeDecl] =
    tparams.mapConserve(x => mapTypeDecl(x))

  private def mapVarDecls(params: List[VarDecl]): List[VarDecl] =
    params.mapConserve(x => mapVarDecl(x))
