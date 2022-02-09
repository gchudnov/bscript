package com.github.gchudnov.bscript.lang.util

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SBlock, SMethod, SStruct, SVar, Scope, Symbol, Type }

object Casting:

  // AST
  implicit class AstOps(ast: AST):
    def asExpr: Either[AstException, Expr] = ast match
      case x: Expr => Right(x)
      case _       => Left(new AstException(s"Cannot cast AST to Expr"))

    def asBlock: Either[AstException, Block] = ast match
      case x: Block => Right(x)
      case _        => Left(new AstException(s"Cannot cast AST to Block"))

    def asMethodDecl: Either[AstException, MethodDecl] = ast match
      case x: MethodDecl => Right(x)
      case _             => Left(new AstException(s"Cannot cast AST to MethodDecl"))

    def asArgDecl: Either[AstException, ArgDecl] = ast match
      case x: ArgDecl => Right(x)
      case _          => Left(new AstException(s"Cannot cast AST to ArgDecl"))

    def asFieldDecl: Either[AstException, FieldDecl] = ast match
      case x: FieldDecl => Right(x)
      case _            => Left(new AstException(s"Cannot cast AST to FieldDecl"))

    def asLValue: Either[AstException, LValue] = ast match
      case x: LValue => Right(x)
      case _         => Left(new AstException(s"Cannot cast AST to LValue"))

  // Scope
  implicit class ScopeOps(scope: Scope):
    def asSMethod: Either[AstException, SMethod] =
      scope match
        case m: SMethod => Right(m)
        case _          => Left(new AstException(s"Cannot cast Scope '${scope.name}' of type '${scopeKind}' to a SMethod"))

    def asSStruct: Either[AstException, SStruct] =
      scope match
        case s: SStruct => Right(s)
        case _          => Left(new AstException(s"Cannot cast Scope '${scope.name}' of type '${scopeKind}' to a SStruct"))

    def asSBlock: Either[AstException, SBlock] =
      scope match
        case b: SBlock => Right(b)
        case _         => Left(new AstException(s"Cannot cast Scope '${scope.name}' of type '${scopeKind}' to a SBlock"))

    def scopeKind: String = scope match
      case _: SMethod => "Method"
      case _: SStruct => "Struct"
      case _: SBlock  => "Block"
      case _          => "?"

  // Symbol
  implicit class SymbolOps(sym: Symbol):

    def asSVar: Either[AstException, SVar] =
      sym match
        case s: SVar => Right(s)
        case _       => Left(new AstException(s"Cannot cast Symbol '${sym.name}' of type '${symbolKind}' to a SVar"))

    def asType: Either[AstException, Symbol with Type] =
      sym match
        case x: Type => Right(x)
        case _       => Left(new AstException(s"Cannot cast Symbol '${sym.name}' of type '${symbolKind}' to a Type"))

    def asSMethod: Either[AstException, SMethod] =
      sym match
        case s: SMethod => Right(s)
        case _          => Left(new AstException(s"Cannot cast Symbol '${sym.name}' of type '${symbolKind}' to a SMethod"))

    def asSStruct: Either[AstException, SStruct] =
      sym match
        case s: SStruct => Right(s)
        case _          => Left(new AstException(s"Cannot cast Symbol '${sym.name}' of type '${symbolKind}' to a SStruct"))

    def symbolKind: String = sym match
      case _: SVar    => "SVar"
      case _: SMethod => "SMethod"
      case _: SStruct => "SStruct"
      case _: Type    => "Type"
      case _          => "?"

  // Type
  implicit class TypeOps(t: Type):

    def asStruct: Either[AstException, SStruct] =
      t match
        case x: SStruct => Right(x)
        case _          => Left(new AstException(s"Cannot cast Type '${t.name}' to a SStruct"))

    /**
     * Returns either a Type of the expression inside of DeclType OR a Type itself
     * @return
     */
    def declType: Type =
      t match
        case DeclType(expr) =>
          expr.evalType
        case _ =>
          t
