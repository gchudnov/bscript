package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*

import com.github.gchudnov.bscript.lang.ast.decls.MethodDecl

import com.github.gchudnov.bscript.lang.ast.decls.StructDecl/
import com.github.gchudnov.bscript.lang.ast.decls.VarDecl**
 * Filters AST
 *
 * If predicate returns 'true', the node is kept, otherwise it is removed.
 *
 * Usage:
 * {{{
 *
 * }}}
 */
trait AstFilter:

  def isKeep(ast: AST): Boolean

  def filterAST(ast: AST): Option[AST] =
    ast match
      case a: Stat =>
        filterStat(a)
      case a: TypeAST =>
        filterTypeAST(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def filterStat(ast: Stat): Option[Stat] =
    ast match
      case a: Expr =>
        filterExpr(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def filterTypeAST(ast: TypeAST): Option[TypeAST] =
    ast match
      case a @ Auto() =>
        if isKeep(ast) then Some(a) else None
      case a @ TypeId(name) =>
        if isKeep(a) then Some(a) else None
      case a @ Applied(aType, args) =>
        for
          aType1 <- filterTypeAST(aType)
          args1   = filterTypeASTs(args)
        yield a.copy(aType = aType1, args = args1)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def filterExpr(ast: Expr): Option[Expr] =
    ast match
      case a: Ref =>
        filterRef(a)
      case a: Decl =>
        filterDecl(a)
      case a @ Assign(lhs, rhs) =>
        for
          lhs1 <- filterRef(lhs)
          rhs1 <- filterExpr(rhs)
        yield a.copy(lhs = lhs1, rhs = rhs1)
      case a @ Block(exprs) =>
        Some(a.copy(exprs = filterExprs(exprs)))
      case a @ Call(id, args) =>
        for
          id1  <- filterRef(id)
          args1 = filterExprs(args)
        yield a.copy(id = id1, args = args1)
      case a @ Compiled(callback, retType) =>
        for retType1 <- filterTypeAST(retType)
        yield a.copy(retType = retType1)
      case a @ If(cond, then1, else1) =>
        for
          cond2 <- filterExpr(cond)
          then2 <- filterExpr(then1)
          else2 <- filterExpr(else1)
        yield a.copy(cond = cond2, then1 = then2, else1 = else2)
      case a @ Init() =>
        Some(a)
      case a @ Literal(const) =>
        Some(a)
      case a: Col =>
        filterCol(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def filterRef(ast: Ref): Option[Ref] =
    ast match
      case a @ Access(x, y) =>
        for
          x1 <- filterRef(x)
          y1 <- filterA(y)
        yield a.copy(a = x1, b = y1)
      case a @ Id(name) =>
        Some(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  def filterDecl(ast: Decl): Option[Decl] =
    ast match
      case a @ MethodDecl(name, tparams, params, retType, body) =>
        for
          retType1 <- filterTypeAST(retType)
          body1    <- filterA(body)
          tparams1  = filterAs(tparams)
          params1   = filterAs(params)
        yield a.copy(tparams = tparams1, params = params1, retType = retType1, body = body1)
      case a @ StructDecl(name, tfields, fields) =>
        Some(a.copy(tfields = filterAs(tfields), fields = filterAs(fields)))
      case a @ VarDecl(name, vType, expr) =>
        for
          vType1 <- filterTypeAST(vType)
          expr1  <- filterExpr(expr)
        yield a.copy(vType = vType1, expr = expr1)
      case a @ TypeDecl(name) =>
        Some(a)
      case other =>
        throw new MatchError(s"Unsupported AST type: ${other}")

  // TODO: it is not clear if we need to have Col / Vec / Dict at all
  def filterCol(ast: Col): Option[Col] =
    ???

  def filterASTs(asts: List[AST]): List[AST] =
    asts.flatMap(x => filterAST(x))

  def filterTypeASTs(asts: List[TypeAST]): List[TypeAST] =
    asts.flatMap(x => filterTypeAST(x))

  def filterExprs(asts: List[Expr]): List[Expr] =
    asts.flatMap(x => filterExpr(x))

  def filterA[A <: AST](ast: A): Option[A] =
    filterAST(ast).asInstanceOf[Option[A]]

  def filterAs[A <: AST](asts: List[A]): List[A] =
    asts.mapConserve(x => filterAST(x)).asInstanceOf[List[A]]
