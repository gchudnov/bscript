package com.github.gchudnov.bscript.lang.func

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*

import com.github.gchudnov.bscript.lang.ast.lit.CollectionLit
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id
import com.github.gchudnov.bscript.lang.ast.refs.Ref
import com.github.gchudnov.bscript.lang.ast.decls.BuiltInDecl

/**
 * Filters AST
 *
 * If predicate returns 'true', the node is kept, otherwise it is removed.
 *
 * Usage:
 * {{{
 *
 * }}}
 */
trait ASTFilter:

  def isKeep(ast: AST): Boolean

  def filterAST(ast: AST): Option[AST] =
    ast match
      case a: Stat =>
        filterStat(a)
      case a: TypeAST =>
        filterTypeAST(a)
      case other =>
        throw new MatchError(s"Unsupported AST: ${other}")

  private def filterStat(ast: Stat): Option[Stat] =
    ast match
      case a: Expr =>
        filterExpr(a)
      case other =>
        throw new MatchError(s"Unsupported Stat: ${other}")

  private def filterTypeAST(ast: TypeAST): Option[TypeAST] =
    ast match
      case a: Auto =>
        if isKeep(a) then Some(a) else None
      case a: TypeId =>
        if isKeep(a) then Some(a) else None
      case a: ByName =>
        for
          aType  <- filterTypeAST(a.aType)
          byName <- if isKeep(a) then Some(a.copy(aType = aType)) else None
        yield byName
      case a: RealType =>
        filterRealType(a)
      case other =>
        throw new MatchError(s"Unsupported TypeAST: ${other}")

  private def filterRealType(ast: RealType): Option[RealType] =
    ast match
      case a: GenericType =>
        if isKeep(a) then Some(a) else None
      case a: BuiltInType =>
        if isKeep(a) then Some(a) else None
      case a: VecType =>
        for
          elemType <- filterTypeAST(a.elemType)
          vecType  <- if isKeep(a) then Some(a.copy(elemType = elemType)) else None
        yield vecType
      case a: MapType =>
        for
          keyType   <- filterTypeAST(a.keyType)
          valueType <- filterTypeAST(a.valueType)
          mapType   <- if isKeep(a) then Some(a.copy(keyType = keyType, valueType = valueType)) else None
        yield mapType
      case a: StructType =>
        val tfields = filterTypeDecls(a.tfields)
        val fields  = filterVarDecls(a.fields)
        for structType <- if isKeep(a) then Some(a.copy(tfields = tfields, fields = fields)) else None
        yield structType
      case a: MethodType =>
        val tparams = filterTypeDecls(a.tparams)
        val params  = filterVarDecls(a.params)
        for
          retType    <- filterTypeAST(a.retType)
          methodType <- if isKeep(a) then Some(a.copy(tparams = tparams, params = params, retType = retType)) else None
        yield methodType
      case other =>
        throw new MatchError(s"Unsupported RealType: ${other}")

  private def filterExpr(ast: Expr): Option[Expr] =
    ast match
      case a: Ref =>
        filterRef(a)
      case a: Decl =>
        filterDecl(a)
      case a: Annotated =>
        val tparams = filterTypeDecls(a.tparams)
        val params  = filterExprs(a.params)
        for
          expr      <- filterExpr(a.expr)
          id        <- filterRef(a.id)
          annotated <- if isKeep(a) then Some(a.copy(expr = expr, id = id, tparams = tparams, params = params)) else None
        yield annotated
      case a: Assign =>
        for
          lhs    <- filterRef(a.lhs)
          rhs    <- filterExpr(a.rhs)
          assign <- if isKeep(a) then Some(a.copy(lhs = lhs, rhs = rhs)) else None
        yield assign
      case a: Block =>
        val exprs = filterExprs(a.exprs)
        for block <- if isKeep(a) then Some(a.copy(exprs = exprs)) else None
        yield block
      case a: Call =>
        for
          id   <- filterRef(a.id)
          args  = filterExprs(a.args)
          call <- if isKeep(a) then Some(a.copy(id = id, args = args)) else None
        yield call
      case a: Compiled =>
        for
          retType  <- filterTypeAST(a.retType)
          compiled <- if isKeep(a) then Some(a.copy(retType = retType)) else None
        yield compiled
      case a: If =>
        for
          cond  <- filterExpr(a.cond)
          then1 <- filterExpr(a.then1)
          else1 <- filterExpr(a.else1)
          if1   <- if isKeep(a) then Some(a.copy(cond = cond, then1 = then1, else1 = else1)) else None
        yield if1
      case a: Init =>
        if isKeep(a) then Some(a) else None
      case a: Return =>
        for
          expr <- filterExpr(a.expr)
          ret  <- if isKeep(a) then Some(a.copy(expr = expr)) else None
        yield ret
      case a: Pair =>
        for
          key      <- filterExpr(a.value)
          value    <- filterExpr(a.value)
          keyValue <- if isKeep(a) then Some(a.copy(key = key, value = value)) else None
        yield keyValue
      case a: Lit =>
        filterLit(a)
      case other =>
        throw new MatchError(s"Unsupported Expr: ${other}")

  private def filterRef(ast: Ref): Option[Ref] =
    ast match
      case a: Access =>
        for
          x1     <- filterRef(a.a)
          y1     <- filterRef(a.b).map(_.asInstanceOf[Id])
          access <- if isKeep(a) then Some(a.copy(a = x1, b = y1)) else None
        yield access
      case a: Id =>
        if isKeep(a) then Some(a) else None
      case other =>
        throw new MatchError(s"Unsupported Ref: ${other}")

  private def filterDecl(ast: Decl): Option[Decl] =
    ast match
      case a: MethodDecl =>
        for
          mType      <- filterTypeAST(a.aType).map(_.asInstanceOf[MethodType])
          body       <- filterExpr(a.body).map(_.asInstanceOf[Block])
          methodDecl <- if isKeep(a) then Some(a.copy(aType = mType, body = body)) else None
        yield methodDecl
      case a: StructDecl =>
        for
          sType      <- filterTypeAST(a.aType).map(_.asInstanceOf[StructType])
          structDecl <- if isKeep(a) then Some(a.copy(aType = sType)) else None
        yield structDecl
      case a: VarDecl =>
        for
          vType   <- filterTypeAST(a.aType)
          expr    <- filterExpr(a.expr)
          varDecl <- if isKeep(a) then Some(a.copy(aType = vType, expr = expr)) else None
        yield varDecl
      case a: TypeDecl =>
        if isKeep(a) then Some(a) else None
      case a: BuiltInDecl =>
        if isKeep(a) then Some(a) else None
      case other =>
        throw new MatchError(s"Unsupported Decl: ${other}")

  private def filterLit(ast: Lit): Option[Lit] =
    ast match
      case a: ConstLit =>
        if isKeep(a) then Some(a) else None
      case a: CollectionLit =>
        for
          cType    <- filterTypeAST(a.cType)
          elems     = filterExprs(a.elems)
          groupLit <- if isKeep(a) then Some(a.copy(cType = cType, elems = elems)) else None
        yield groupLit
      case a: MethodLit =>
        for
          mType     <- filterTypeAST(a.mType).map(_.asInstanceOf[MethodType])
          body      <- filterExpr(a.body).map(_.asInstanceOf[Block])
          methodLit <- if isKeep(a) then Some(a.copy(mType = mType, body = body)) else None
        yield methodLit
      case other =>
        throw new MatchError(s"Unsupported Lit: ${other}")

  private def filterTypeDecls(typeDecls: List[TypeDecl]): List[TypeDecl] =
    typeDecls.flatMap(it => filterDecl(it).map(_.asInstanceOf[TypeDecl]))

  private def filterVarDecls(typeDecls: List[VarDecl]): List[VarDecl] =
    typeDecls.flatMap(it => filterDecl(it).map(_.asInstanceOf[VarDecl]))

  private def filterExprs(exprs: List[Expr]): List[Expr] =
    exprs.flatMap(it => filterExpr(it))
