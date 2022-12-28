package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*

/**
 * Maps AST
 *
 * Usage:
 * {{{
 *
 * }}}
 */
trait AstMapper:

  def mapAST(ast: AST): AST =
    ast match
      case a: Stat =>
        mapStat(a)
      case a: TypeAST =>
        mapTypeAST(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapStat(ast: Stat): Stat =
    ast match
      case a: Expr =>
        mapExpr(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapTypeAST(ast: TypeAST): TypeAST =
    ast match
      case a @ Auto() =>
        ast
      case a @ TypeId(name) =>
        a
      case a @ Applied(aType, args) =>
        a.copy(aType = mapTypeAST(aType), args = mapTypeASTs(args))
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapExpr(ast: Expr): Expr =
    ast match
      case a: Ref =>
        mapRef(a)
      case a: Decl =>
        mapDecl(a)
      case a @ Assign(lhs, rhs) =>
        a.copy(lhs = mapRef(lhs), rhs = mapExpr(rhs))
      case a @ Block(exprs) =>
        a.copy(exprs = mapExprs(exprs))
      case a @ Call(id, args) =>
        a.copy(id = mapRef(id), args = mapExprs(args))
      case a @ Compiled(callback, retType) =>
        a.copy(retType = mapTypeAST(retType))
      case a @ If(cond, then1, else1) =>
        a.copy(cond = mapExpr(cond), then1 = mapExpr(then1), else1 = mapExpr(else1))
      case a @ Init() =>
        a
      case a @ Literal(const) =>
        a
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapRef(ast: Ref): Ref =
    ast match
      case a @ Access(x, y) =>
        a.copy(a = mapRef(x), b = mapA(y))
      case a @ Id(name) =>
        a
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapDecl(ast: Decl): Decl =
    ast match
      case a @ MethodDecl(name, tparams, params, retType, body) =>
        a.copy(tparams = mapAs(tparams), params = mapAs(params), retType = mapTypeAST(retType), body = mapA(body))
      case a @ StructDecl(name, tfields, fields) =>
        a.copy(tfields = mapAs(tfields), fields = mapAs(fields))
      case a @ VarDecl(name, vType, expr) =>
        a.copy(vType = mapTypeAST(vType), mapExpr(expr))
      case a @ TypeDecl(name) =>
        a
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def mapASTs(asts: List[AST]): List[AST] =
    asts.mapConserve(x => mapAST(x))

  def mapTypeASTs(asts: List[TypeAST]): List[TypeAST] =
    asts.mapConserve(x => mapTypeAST(x))

  def mapExprs(asts: List[Expr]): List[Expr] =
    asts.mapConserve(x => mapExpr(x))

  def mapA[A <: AST](ast: A): A =
    mapAST(ast).asInstanceOf[A]

  def mapAs[A <: AST](asts: List[A]): List[A] =
    asts.mapConserve(x => mapAST(x)).asInstanceOf[List[A]]
