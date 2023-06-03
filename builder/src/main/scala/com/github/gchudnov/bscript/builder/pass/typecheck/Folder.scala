package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.func.AstFolder

import com.github.gchudnov.bscript.lang.ast.decls.VarDecl
import com.github.gchudnov.bscript.builder.pass.typecheck.PassState

import scala.annotation.tailrec

/**
  * Fold the AST to do type checking.
  */
private[builder] final class Folder() extends AstFolder[PassState]:

  override def foldAST(s: PassState, ast: AST): PassState =
    ast match

      case x @ ConstLit(const) =>
        foldOverAST(s, x)

private[builder] object Folder:

  def make(): Folder =
    new Folder()


// private[typecheck] final class TypeCheckFolder() {} // extends TreeVisitor[TypeCheckState, TypeCheckState]:
//   import Casting.*
//   import TypeCheckVisitor.*

//   private val commonTable: CommonResult                = typeCheckLaws.commonTable
//   private val additionTable: AdditionResult            = typeCheckLaws.additionTable
//   private val arithmeticTable: ArithmeticResult        = typeCheckLaws.arithmeticTable
//   private val relationalTable: RelationalResult        = typeCheckLaws.relationalTable
//   private val equalityTable: EqualityResult            = typeCheckLaws.equalityTable
//   private val logicTable: LogicResult                  = typeCheckLaws.logicTable
//   private val unaryArithmeticSet: UnaryArithmeticAllow = typeCheckLaws.unaryArithmeticSet
//   private val unaryLogicSet: UnaryLogicAllow           = typeCheckLaws.unaryLogicSet
//   private val promoteFromToTable: PromoteFromTo        = typeCheckLaws.promoteFromToTable

//   override def visit(s: TypeCheckState, n: Init): Either[Throwable, TypeCheckState] =
//     for
//       ss1 <- Right(s.meta.withEvalType(n, n.iType))
//     yield s.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: UnaryMinus): Either[Throwable, TypeCheckState] =
//     for
//       s1   <- n.expr.visit(s, this)
//       expr <- s1.ast.asExpr
//       exprEvalType <- s1.meta.evalTypeFor(n)
//       _    <- unaryOpTypeCheck(OpName.minus, exprEvalType)
//     yield s1

//   override def visit(s: TypeCheckState, n: Add): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- addOpType(OpName.plus, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Sub): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- binOpType(OpName.minus, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Mul): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- binOpType(OpName.multiply, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Div): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- binOpType(OpName.division, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Mod): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- binOpType(OpName.modulo, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Less): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- relOpType(OpName.less, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: LessEqual): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- relOpType(OpName.lessEqual, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Greater): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- relOpType(OpName.greater, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: GreaterEqual): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- relOpType(OpName.greaterEqual, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Equal): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- eqOpType(OpName.equal, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: NotEqual): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- eqOpType(OpName.notEqual, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Not): Either[Throwable, TypeCheckState] =
//     for
//       s1   <- n.expr.visit(s, this)
//       expr <- s1.ast.asExpr
//       exprEvalType <- s1.meta.evalTypeFor(n)
//       _    <- unaryLogicOpTypeCheck(OpName.not, exprEvalType)
//     yield s1

//   override def visit(s: TypeCheckState, n: And): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- logicOpType(OpName.and, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Or): Either[Throwable, TypeCheckState] =
//     for
//       ls                    <- n.lhs.visit(s, this)
//       rs                    <- n.rhs.visit(ls, this)
//       lValue                <- ls.ast.asExpr
//       rValue                <- rs.ast.asExpr
//       lValueEvalType <- ls.meta.evalTypeFor(lValue)
//       rValueEvalType <- rs.meta.evalTypeFor(rValue)
//       r                     <- logicOpType(OpName.or, lValueEvalType, rValueEvalType)
//       ss1                    = rs.meta.withPromoteToType(lValue, r.lhsPromoteToType).withPromoteToType(rValue, r.rhsPromoteToType).withEvalType(n, r.resultType)
//     yield rs.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: Assign): Either[Throwable, TypeCheckState] =
//     // for
//     //   ls                 <- n.id.visit(s, this)
//     //   rs                 <- n.expr.visit(ls, this)
//     //   lValue             <- ls.ast.asLValue
//     //   rValue             <- rs.ast.asExpr
//     //   lValueEvalType <- ls.meta.evalTypeFor(lValue)
//     //   rValueEvalType <- rs.meta.evalTypeFor(rValue)
//     //   rValuePromoteToType = promoteFromTo(rValueEvalType, lValueEvalType)
//     //   promotedRType <- Either.cond(
//     //                       canAssignTo(rValueEvalType, rValuePromoteToType, lValueEvalType),
//     //                       rValuePromoteToType,
//     //                       new AstException(s"Cannot convert type '${rValueEvalType.name}' to '${lValueEvalType.name}' in the assignment (${Ctx.str(rs.meta, n)})")
//     //                     )
//     //   evalType = types.voidType
//     //   ss1      = rs.meta.withPromoteToType(rValue, promotedRType).withEvalType(n, evalType)
//     // yield rs.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: NothingVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.nothingType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: VoidVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.voidType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: BoolVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.boolType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: IntVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.i32Type)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: LongVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.i64Type)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: FloatVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.f32Type)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: DoubleVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.f64Type)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: DecimalVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.decType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: StrVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.strType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: DateVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.dateType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: DateTimeVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.datetimeType)
//     //   ss1       = s.meta.withEvalType(n, evalType)
//     // yield s.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: StructVal): Either[Throwable, TypeCheckState] =
//     // for
//     //   sStruct <- n.sType.asSStruct
//     //   sTypes  <- s.meta.structTypes(sStruct)
//     //   s1 <- n.value.foldLeft(Right(s): Either[Throwable, TypeCheckState]) { case (acc, (name, expr)) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right(sx) =>
//     //               for
//     //                 lValueType         <- sTypes.get(name).toRight(new AstException(s"Field '${name}' doesn't belong to the struct '${sStruct.name}'"))
//     //                 sy                 <- expr.visit(sx, this)
//     //                 exprN              <- sy.ast.asExpr
//     //                 rValueType         <- sy.meta.evalTypeFor(exprN)
//     //                 rValuePromoteToType = promoteFromTo(rValueType, lValueType)
//     //                 promotedRType <- Either.cond(
//     //                                     canAssignTo(rValueType, rValuePromoteToType, lValueType),
//     //                                     rValuePromoteToType,
//     //                                     new AstException(
//     //                                       s"Cannot convert type '${rValueType.name}' to '${lValueType.name}' in the struct '${sStruct.name}' assignment (${Ctx.str(sy.meta, n)})"
//     //                                     )
//     //                                   )
//     //                 sz = sy.meta.withPromoteToType(exprN, promotedRType)
//     //               yield (sy)
//     //         }
//     //   ss1          = s1.meta.withEvalType(n, n.sType)
//     // yield s1.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: Vec): Either[Throwable, TypeCheckState] =
//     // for
//     //   es <- n.elements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (TypeCheckState, List[Expr])]) { case (acc, expr) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right((sx, exprs)) =>
//     //               for
//     //                 sy    <- expr.visit(sx, this)
//     //                 exprN <- sy.ast.asExpr
//     //               yield (sy, exprs :+ exprN)
//     //         }
//     //   (s1, exprs) = es
//     //   // find common element type; NOTE: we consider the case when types might be different, but compatible.
//     //   optElementType <- exprs.foldLeft(Right(None): Either[Throwable, Option[Type]]) { case (acc, expr) =>
//     //                     for {
//     //                       exprEvalType <- s1.meta.evalTypeFor(expr)
//     //                       maybeType <- acc match
//     //                        case Right(None) => Right(Some(exprEvalType))
//     //                        case Right(Some(t)) => Right(promoteFromTo(t, exprEvalType).orElse(promoteFromTo(exprEvalType, t)).orElse(Some(t)))
//     //                     } yield maybeType
//     //                    }
//     //   // TODO: finish it
//     //   // promotedExprs <- optElementType.fold(Right(Seq.empty[Expr]): Either[Throwable, Seq[Expr]]) { elementType =>
//     //   //                    val promotedExprTypes =  Transform.sequence(exprs.map(expr => s1.meta.evalTypeFor(expr).map(exprEvalType => promoteFromTo(exprEvalType, elementType))))

//     //   //                    val candidateExprs = exprs.map(expr => expr.withPromoteToType(promoteFromTo(expr.evalType, elementType)))
//     //   //                    Transform.sequence(
//     //   //                      candidateExprs.map(expr =>
//     //   //                        Either.cond(
//     //   //                          canAssignTo(expr.evalType, expr.promoteToType, elementType),
//     //   //                          expr,
//     //   //                          new AstException(s"Element of the collection '${expr.evalType.name}' is not compatible with the collection type '${elementType.name}'")
//     //   //                        )
//     //   //                      )
//     //   //                    )
//     //   //                  }
//     //   // elementType = optElementType.getOrElse(n.elementType)
//     //   // evalType    = VectorType(elementType)
//     //   // n1          = n.copy(elements = promotedExprs, elementType = elementType, evalType = evalType)
//     //   ss1         = s1.meta // .redefineASTScope(n, n1)
//     // yield s1.copy(meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: Var): Either[Throwable, TypeCheckState] =
//     for
//       sVar  <- n.symbol.asSVar
//       sType <- s.meta.typeFor(sVar)
//       ss1    = s.meta.withEvalType(n, sType)
//     yield s.copy(meta = ss1)

//   override def visit(s: TypeCheckState, n: ArgDecl): Either[Throwable, TypeCheckState] =
//     // for
//     //   sVar                <- n.symbol.asSVar
//     //   st                  <- visitType(s, n.aType)
//     //   StateType(s1, aType) = st
//     //   evalType            <- Right(types.voidType)
//     //   n1                   = n.copy(aType = aType, evalType = evalType)
//     //   ss1                  = s1.meta.defineVarType(sVar, aType).redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: VarDecl): Either[Throwable, TypeCheckState] =
//     // for
//     //   sVar                <- n.symbol.asSVar
//     //   st                  <- visitType(s, n.vType)
//     //   StateType(s1, vType) = st
//     //   s2                  <- n.expr.visit(s1, this)
//     //   expr                <- s2.ast.asExpr
//     //   varType              = if vType.declType == types.autoType then expr.evalType else vType.declType
//     //   exprPromoteToType    = promoteFromTo(expr.evalType, varType)
//     //   promotedExpr <- Either.cond(
//     //                     canAssignTo(expr.evalType, exprPromoteToType, varType),
//     //                     expr.withPromoteToType(exprPromoteToType),
//     //                     new AstException(s"Cannot convert type '${expr.evalType.name}' to '${varType.name}' in variable '${n.name}' declaration (${Ctx.str(s2.meta, n)})")
//     //                   )
//     //   evalType = types.voidType
//     //   n1       = n.copy(vType = varType, expr = promotedExpr, evalType = evalType)
//     //   ss1      = s2.meta.defineVarType(sVar, varType).redefineASTScope(n, n1)
//     // yield s2.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: FieldDecl): Either[Throwable, TypeCheckState] =
//     // for
//     //   evalType <- Right(types.voidType)
//     //   n1        = n.copy(evalType = evalType)
//     //   ss1       = s.meta.redefineASTScope(n, n1)
//     // yield s.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: MethodDecl): Either[Throwable, TypeCheckState] =
//     // for
//     //   sMethod <- n.symbol.asSMethod
//     //   sa <- n.params.foldLeft(Right((s, List.empty[ArgDecl])): Either[Throwable, (TypeCheckState, List[ArgDecl])]) { case (acc, argDecl) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right((sx, args)) =>
//     //               for
//     //                 sy   <- argDecl.visit(sx, this)
//     //                 argN <- sy.ast.asArgDecl
//     //               yield (sy, args :+ argN)
//     //         }
//     //   (s1, params)           = sa
//     //   st                    <- visitType(s1, n.retType)
//     //   StateType(s2, retType) = st
//     //   s3                    <- n.body.visit(s2, this)
//     //   body                  <- s3.ast.asBlock
//     //   bodyPromoteToType      = promoteFromTo(body.evalType, retType)
//     //   promotedBody <- Either.cond(
//     //                     canAssignTo(body.evalType.declType, bodyPromoteToType, retType.declType),
//     //                     body.withPromoteToType(bodyPromoteToType),
//     //                     new AstException(
//     //                       s"Cannot convert type '${body.evalType.name}' to '${retType.name}' in the return statement of method '${s3.meta.showMethod(sMethod)}' (${Ctx.str(s3.meta, n)})"
//     //                     )
//     //                   )
//     //   evalType = types.voidType
//     //   n1       = n.copy(params = params, body = promotedBody, evalType = evalType, retType = retType)
//     //   ss1      = s3.meta.defineMethodRetType(sMethod, retType).defineMethodAST(sMethod, n1).redefineASTScope(n, n1)
//     // yield s3.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: StructDecl): Either[Throwable, TypeCheckState] =
//     // for
//     //   sf <- n.fields.foldLeft(Right((s, List.empty[FieldDecl])): Either[Throwable, (TypeCheckState, List[FieldDecl])]) { case (acc, fieldDecl) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right((sx, fields)) =>
//     //               for
//     //                 sy     <- fieldDecl.visit(sx, this)
//     //                 fieldN <- sy.ast.asFieldDecl
//     //               yield (sy, fields :+ fieldN)
//     //         }
//     //   (s1, fields) = sf
//     //   evalType     = types.voidType
//     //   n1           = n.copy(fields = fields, evalType = evalType)
//     //   ss1          = s1.meta.redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: Block): Either[Throwable, TypeCheckState] =
//     // for
//     //   bs <- n.statements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (TypeCheckState, List[Expr])]) { case (acc, expr) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right((sx, exprs)) =>
//     //               for
//     //                 sy    <- expr.visit(sx, this)
//     //                 exprN <- sy.ast.asExpr
//     //               yield (sy, exprs :+ exprN)
//     //         }
//     //   (s1, exprs) = bs
//     //   voidType    = types.voidType
//     //   evalType    = exprs.lastOption.map(_.evalType).getOrElse(voidType)
//     //   n1          = n.copy(statements = exprs, evalType = evalType)
//     //   ss1         = s1.meta.redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   /**
//    * Given g(int x, float y) { ... } and Call g('q', 10), we need to promote 'q' to int and 10 to float.
//    */
//   override def visit(s: TypeCheckState, n: Call): Either[Throwable, TypeCheckState] =
//     // for
//     //   sMethod <- n.id.asSMethod
//     //   sa <- n.args.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (TypeCheckState, List[Expr])]) { case (acc, expr) =>
//     //           acc match
//     //             case Left(ex) => Left(ex)
//     //             case Right((sx, args)) =>
//     //               for
//     //                 sy   <- expr.visit(sx, this)
//     //                 argN <- sy.ast.asExpr
//     //               yield (sy, args :+ argN)
//     //         }
//     //   (s1, callExprs)   = sa
//     //   methodArgTypeMap <- s1.meta.methodArgTypeMap(sMethod)
//     //   methodArgSVars   <- s1.meta.methodArgSVars(sMethod)
//     //   methodArgTypes   <- s1.meta.methodArgTypes(sMethod)
//     //   _ <- Either.cond(
//     //          methodArgTypes.size == callExprs.size,
//     //          (),
//     //          new AstException(s"Not enough arguments for the method ${s.meta.showMethod(sMethod)} call are provided. Expected: ${methodArgTypes.size}, got: ${callExprs.size}")
//     //        )
//     //   promotedCallExprs <- Transform.sequence(
//     //                          methodArgSVars
//     //                            .zip(callExprs)
//     //                            .map { case (methodArgSVar, callArgExpr) =>
//     //                              val methodArgType = methodArgTypeMap(methodArgSVar)
//     //                              val actualArgType = callArgExpr.evalType

//     //                              val argPromoteToType = promoteFromTo(actualArgType, methodArgType)

//     //                              Either.cond(
//     //                                canAssignTo(actualArgType, argPromoteToType, methodArgType),
//     //                                callArgExpr.withPromoteToType(argPromoteToType),
//     //                                new AstException(
//     //                                  s"Cannot convert argument '${methodArgSVar.name}' type from '${actualArgType.name}' to '${methodArgType.name}' in '${s.meta.showMethod(sMethod)}' method call"
//     //                                )
//     //                              )
//     //                            }
//     //                        )

//     //   // for ret type, consider the case when the DeclType is used; we're not propagating callState, but use it for retType deduction.
//     //   callState = methodArgSVars
//     //                 .zip(promotedCallExprs)
//     //                 .foldLeft(s.meta) { case (acc, (sVar, expr)) =>
//     //                   acc.defineVarType(sVar, expr.evalType)
//     //                 }
//     //   methodRetType            <- s1.meta.retTypeFor(sMethod)
//     //   st                       <- visitType(s.copy(meta = callState), methodRetType)
//     //   StateType(_, callRetType) = st
//     //   n1                        = n.copy(args = promotedCallExprs, evalType = callRetType.declType)
//     //   ss1                       = s1.meta.redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: If): Either[Throwable, TypeCheckState] =
//     // for
//     //   cs                 <- n.cond.visit(s, this)
//     //   condExpr           <- cs.ast.asExpr
//     //   ts                 <- n.then1.visit(cs, this)
//     //   thenExpr           <- ts.ast.asExpr
//     //   es                 <- Transform.sequence(n.else1.map(_.visit(ts, this)))
//     //   optElseExpr        <- Transform.sequence(es.map(_.ast.asExpr))
//     //   s1                  = es.getOrElse(ts)
//     //   opName              = "if"
//     //   _                  <- unaryLogicOpTypeCheck(opName, condExpr.evalType)
//     //   optResType         <- Transform.sequence(optElseExpr.map(elseExpr => comOpType(opName, thenExpr.evalType, elseExpr.evalType)))
//     //   evalType            = optResType.map(_.resultType).getOrElse(thenExpr.evalType)
//     //   promotedThenExpr    = optResType.map(r => thenExpr.withPromoteToType(r.lhsPromoteToType)).getOrElse(thenExpr)
//     //   optPromotedElseExpr = optElseExpr.flatMap(elseExpr => optResType.map(r => elseExpr.withPromoteToType(r.rhsPromoteToType)))
//     //   n1                  = n.copy(cond = condExpr, then1 = promotedThenExpr, else1 = optPromotedElseExpr, evalType = evalType)
//     //   ss1                 = s1.meta.redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: Access): Either[Throwable, TypeCheckState] =
//     // for
//     //   ls     <- n.a.visit(s, this)
//     //   lValue <- ls.ast.asLValue
//     //   rs     <- n.b.visit(ls, this)
//     //   rValue <- rs.ast.asLValue
//     //   _ <- Either.cond(
//     //          lValueEvalType.isInstanceOf[SStruct],
//     //          (),
//     //          new AstException(s"Left operand of the access 'a.b' expression must be a struct, got '${lValueEvalType.name}' instead.")
//     //        )
//     //   n1  = n.copy(a = lValue, b = rValue)
//     //   ss1 = rs.meta.redefineASTScope(n, n1).withEvalType(n1, rValueEvalType)
//     // yield rs.copy(ast = n1, meta = ss1)
//     ???

//   override def visit(s: TypeCheckState, n: CompiledExpr): Either[Throwable, TypeCheckState] =
//     // for
//     //   st                    <- visitType(s, n.retType)
//     //   StateType(s1, retType) = st
//     //   n1                     = n.copy(retType = retType, evalType = retType.declType)
//     //   ss1                    = s1.meta.redefineASTScope(n, n1)
//     // yield s1.copy(ast = n1, meta = ss1)
//     ???

//   private def comOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(commonTable.tt, opName, lhs, rhs).left.map(_ => new AstException(s"Cannot find a common type for '${lhs.name}' and '${rhs.name}' in ${opName}"))

//   private def binOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(arithmeticTable.tt, opName, lhs, rhs)

//   private def addOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(additionTable.tt, opName, lhs, rhs)

//   private def eqOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(equalityTable.tt, opName, lhs, rhs)

//   private def relOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(relationalTable.tt, opName, lhs, rhs)

//   private def logicOpType(opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     binaryResultType(logicTable.tt, opName, lhs, rhs)

//   private def unaryOpTypeCheck(opName: String, t: Type): Either[Throwable, Unit] =
//     unaryTypeCheck(unaryArithmeticSet.ts, opName, t)

//   private def unaryLogicOpTypeCheck(opName: String, t: Type): Either[Throwable, Unit] =
//     unaryTypeCheck(unaryLogicSet.ts, opName, t)

//   private def binaryResultType(tt: TypeTable, opName: String, lhs: Type, rhs: Type): Either[Throwable, BinaryResultType] =
//     // for resultType <- tt.get((lhs, rhs)).toRight(new AstException(s"Operation ${opName}('${lhs.name}', '${rhs.name}') is not supported"))
//     // yield
//     //   val lhsPromoteToType = promoteFromTo(lhs, rhs)
//     //   val rhsPromoteToType = promoteFromTo(rhs, lhs)
//     //   BinaryResultType(resultType = resultType, lhsPromoteToType = lhsPromoteToType, rhsPromoteToType = rhsPromoteToType)
//     ???

//   private def unaryTypeCheck(ts: TypeSet, opName: String, t: Type): Either[Throwable, Unit] =
//     Either.cond(ts.contains(t), (), new AstException(s"Operation ${opName}('${t.name}') is not supported"))

//   /**
//    * To assign, either:
//    *
//    * 1) (src, dst) types must be the same
//    *
//    * OR
//    *
//    * 2) (promoted, dst) types must be the same
//    *
//    * OR
//    *
//    * 3) (dst) type must be auto
//    */
//   // @tailrec
//   private def canAssignTo(srcEvalType: Type, srcPromoteToType: Option[Type], dstType: Type): Boolean =
//     // (srcEvalType, dstType) match
//     //   case (VectorType(sType), VectorType(dType)) =>
//     //     canAssignTo(sType, srcPromoteToType, dType)
//     //   case (sType, DeclType(expr)) =>
//     //     canAssignTo(sType, srcPromoteToType, expr.evalType)
//     //   case (sType, dType) =>
//     //     ((srcEvalType.name == dstType.name) || (srcPromoteToType.exists(promoted => promoted.name == dstType.name)) || (dstType == types.autoType))
//     ???

//   /**
//    * Checks the result of promotion, None if promotion is not needed.
//    */
//   private def promoteFromTo(fromType: Type, toType: Type): Option[Type] =
//     promoteFromToTable.tt.get((fromType, toType))

//   private def visitType(s: TypeCheckState, t: Type): Either[Throwable, StateType] =
//     t match
//       case VectorType(elementType) =>
//         visitType(s, elementType).map(st => st.copy(xType = VectorType(st.xType)))
//       case DeclType(expr) =>
//         expr
//           .visit(s, this)
//           .flatMap { s1 =>
//             for newExpr <- s1.ast.asExpr
//             yield StateType(s1, DeclType(newExpr))
//           }
//       case _ =>
//         Right(StateType(s, t))

// private[builder] object TypeCheckVisitor:

//   def make(typeCheckLaws: TypeCheckLaws): TypeCheckVisitor =
//     new TypeCheckVisitor(typeCheckLaws)

//   final case class TypeCheckState(
//     ast: AST,
//     meta: Meta
//   )

//   object TypeCheckState:
//     def make(ast: AST, meta: Meta): TypeCheckState =
//       TypeCheckState(
//         ast = ast,
//         meta = meta
//       )

//   final case class BinaryResultType(resultType: Type, lhsPromoteToType: Option[Type], rhsPromoteToType: Option[Type])

//   private final case class StateType(
//     state: TypeCheckState,
//     xType: Type
//   )
