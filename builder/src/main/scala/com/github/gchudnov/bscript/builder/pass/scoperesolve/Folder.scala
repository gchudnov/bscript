package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.func.ASTFolder

import com.github.gchudnov.bscript.lang.ast.decls.VarDecl
import com.github.gchudnov.bscript.builder.pass.scoperesolve.PassState

/**
  * Fold the AST to resolve scopes
  */
private[builder] final class Folder() extends ASTFolder[PassState]:

  override def foldAST(s: PassState, ast: AST): PassState =
    ast match
      // case x: Access =>
      //   // foldOverAST(a, x)
      //   ???

      // case x @ VarDecl(name, vType, expr) =>
      //   foldOverAST(s.resolveVarDecl(name, vType, x), x)



      // case x: Assign =>
      //   foldOverAST(a, x)
      case x: Block =>
        foldOverAST(s, x)
      // case x @ Literal(_) =>
      //   foldOverAST(a, x)
      // case x @ Call(id, args) =>
      //   foldOverAST(a.bind(x), x)
      // case x @ Compiled(_, retType) =>
      //   foldOverAST(a, x)
      // case x: If =>
      //   foldOverAST(a, x)
      case x @ Init() =>
        foldOverAST(s, x)

      case x @ ConstLit(const) =>
        foldOverAST(s, x)

      case x @ TypeId(name) =>
        foldOverAST(s, x)

      // case x @ MethodDecl(retType, name, _, _) =>
      //   foldOverAST(a.define(SMethod(name)).push(), x).pop()
      // case x @ StructDecl(name, _) =>
      //   foldOverAST(a.define(SStruct(name)).push(), x).pop()
      // case x @ Var(sym) =>
      //   foldOverAST(a, x)
      // case x @ Vec(_, elementType) =>
      //   foldOverAST(a, x)

      case other =>
        throw new MatchError(s"Unsupported AST type in Resolve-Folder: ${other}")


private[builder] object Folder:

  def make(): Folder =
    new Folder()

//   override def visit(s: PassState, n: Access): Either[Throwable, PassState] =

//     // Find 'y'-symbol in 'x'-struct
//     def member(xVar: SVar, y: Var): Either[Throwable, SVar] =
//       for
//         xType <- s.meta.typeFor(xVar).flatMap(_.asSStruct)
//         yVar  <- s.meta.resolveMember(y.symbol.name, xType).left.map(_ => new AstException(s"Symbol '${xVar.name}.${y.symbol.name}' cannot be resolved")).flatMap(_.asSVar)
//       yield yVar

//     def iterate(sx: Meta, n1: Access): Either[Throwable, (Meta, (SVar, Access))] =
//       (n1.a, n1.b) match
//         case (x: Var, y: Var) =>
//           // 'x' must be a Struct Type, 'y' is a field of struct 'x'.
//           for
//             scopeX <- s.meta.scopeFor(x).flatMap(_.asSBlock)
//             xVar   <- s.meta.resolve(x.symbol.name, scopeX).flatMap(_.asSVar)
//             yVar   <- member(xVar, y)
//             nx      = x.copy(symbol = xVar)
//             ny      = y.copy(symbol = yVar)
//             n2      = n1.copy(a = nx, b = ny)
//             sy      = sx.redefineASTScope(n1, n2).redefineASTScope(x, nx).redefineASTScope(y, ny).ensureNoAST(n)
//           yield (sy, (yVar, n2))

//         case (xx: Access, y: Var) =>
//           iterate(sx, xx).flatMap { case (sy, (xVar, xAcc)) =>
//             for
//               yVar <- member(xVar, y)
//               ny    = y.copy(symbol = yVar)
//               n2    = n1.copy(a = xAcc, b = ny)
//               sz    = sy.redefineASTScope(n1, n2).redefineASTScope(y, ny).ensureNoAST(n)
//             yield (sz, (yVar, n2))
//           }

//         case _ =>
//           Left(new AstException(s"Cannot build scopes for Access('${n.a}', '${n.b}'); The expected Access structure is Access(Var, Var) or Access(Access, Var)."))

//     for
//       t             <- iterate(s.meta, n)
//       (ss1, (_, n1)) = t
//     yield s.copy(meta = ss1, ast = n1)

//   override def visit(s: PassState, n: MethodDecl): Either[Throwable, PassState] =
//     for
//       scope <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       sa <- n.params.foldLeft(Right((s, List.empty[ArgDecl])): Either[Throwable, (PassState, List[ArgDecl])]) { case (acc, argDecl) =>
//               acc match
//                 case Left(ex) => Left(ex)
//                 case Right((sx, args)) =>
//                   for
//                     sy   <- argDecl.visit(sx, this)
//                     argN <- sy.ast.asArgDecl
//                   yield (sy, args :+ argN)
//             }
//       (s1, params)           = sa
//       sMethod               <- s1.meta.resolveMember(n.name, scope).flatMap(_.asSMethod)
//       st                    <- visitType(s1, scope, n.retType)
//       StateType(s2, retType) = st
//       ss1                    = s2.meta.defineMethodRetType(sMethod, retType)
//       s3                     = s2.copy(meta = ss1)
//       s4                    <- n.body.visit(s3, this)
//       body                  <- s4.ast.asBlock
//       n1                     = n.copy(retType = retType, body = body, params = params)
//       ss2                    = s4.meta.defineMethodAST(sMethod, n1).redefineASTScope(n, n1).ensureNoAST(n)
//     yield s3.copy(ast = n1, meta = ss2)

//   override def visit(s: PassState, n: StructDecl): Either[Throwable, PassState] =
//     for
//       scope   <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       _       <- s.meta.resolve(n.name, scope).flatMap(_.asType) // NOTE: Struct is a Type as well; this line is used to assert this fact here.
//       sStruct <- s.meta.resolve(n.name, scope).flatMap(_.asSStruct)
//       sf <- n.fields.foldLeft(Right((s, List.empty[FieldDecl])): Either[Throwable, (PassState, List[FieldDecl])]) { case (acc, fieldDecl) =>
//               acc match
//                 case Left(ex) => Left(ex)
//                 case Right((sx, fields)) =>
//                   for
//                     sy     <- fieldDecl.visit(sx, this)
//                     fieldN <- sy.ast.asFieldDecl
//                   yield (sy, fields :+ fieldN)
//             }
//       (s1, fields) = sf
//       n1           = n.copy(fields = fields)
//       ss1          = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: Var): Either[Throwable, PassState] =
//     for
//       scope <- s.meta.scopeFor(n)
//       sVar  <- s.meta.resolve(n.symbol.name, scope).flatMap(_.asSVar)
//       n1     = n.copy(symbol = sVar)
//       ss1    = s.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: Assign): Either[Throwable, PassState] =
//     for
//       ls    <- n.id.visit(s, this)
//       rs    <- n.expr.visit(ls, this)
//       id1   <- ls.ast.asLValue
//       expr1 <- rs.ast.asExpr
//       n1     = n.copy(id = id1, expr = expr1)
//       ss1    = rs.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield rs.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: StructVal): Either[Throwable, PassState] =
//     for
//       scope               <- s.meta.scopeFor(n)
//       st                  <- visitType(s, scope, n.sType)
//       StateType(s1, sType) = st
//       sv <- n.value.foldLeft(Right((s1, Map.empty[String, Expr])): Either[Throwable, (PassState, Map[String, Expr])]) { case (acc, (name, expr)) =>
//               acc match
//                 case Left(ex) => Left(ex)
//                 case Right((sx, map)) =>
//                   for
//                     sy    <- expr.visit(sx, this)
//                     exprN <- sy.ast.asExpr
//                   yield (sy, map + (name -> exprN))
//             }
//       (s2, value1) = sv
//       n1           = n.copy(sType = sType, value = value1)
//       ss1          = s2.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s2.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: Vec): Either[Throwable, PassState] =
//     for
//       se <- n.elements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (PassState, List[Expr])]) { case (acc, expr) =>
//               acc match
//                 case Left(ex) => Left(ex)
//                 case Right((sx, exprs)) =>
//                   for
//                     sy    <- expr.visit(sx, this)
//                     exprN <- sy.ast.asExpr
//                   yield (sy, exprs :+ exprN)
//             }
//       (s1, exprs) = se
//       n1          = n.copy(elements = exprs) // NOTE: element type is deduced in Phase #3
//       ss1         = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: Call): Either[Throwable, PassState] =
//     for
//       scope   <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       sMethod <- s.meta.resolve(n.id.name, scope).flatMap(_.asSMethod)
//       sa <- n.args.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (PassState, List[Expr])]) { case (acc, expr) =>
//               acc match
//                 case Left(ex) => Left(ex)
//                 case Right((sx, args)) =>
//                   for
//                     sy   <- expr.visit(sx, this)
//                     argN <- sy.ast.asExpr
//                   yield (sy, args :+ argN)
//             }
//       (s1, args) = sa
//       n1         = n.copy(id = sMethod, args = args)
//       ss1        = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: If): Either[Throwable, PassState] =
//     for
//       cs          <- n.cond.visit(s, this)
//       ts          <- n.then1.visit(cs, this)
//       es          <- Transform.sequence(n.else1.map(_.visit(ts, this)))
//       condExpr    <- cs.ast.asExpr
//       thenExpr    <- ts.ast.asExpr
//       optElseExpr <- Transform.sequence(es.map(_.ast.asExpr))
//       s1           = es.getOrElse(ts)
//       n1           = n.copy(cond = condExpr, then1 = thenExpr, else1 = optElseExpr)
//       ss1          = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   override def visit(s: PassState, n: CompiledExpr): Either[Throwable, PassState] =
//     for
//       scope                 <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       st                    <- visitType(s, scope, n.retType)
//       StateType(s1, retType) = st
//       n1                     = n.copy(retType = retType)
//       ss1                    = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   private def visitType(s: PassState, scope: Scope, t: Type): Either[Throwable, StateType] =
//     t match
//       case VectorType(elementType) =>
//         visitType(s, scope, elementType).map(st => st.copy(xType = VectorType(st.xType)))
//       case DeclType(expr) =>
//         expr
//           .visit(s, this)
//           .flatMap { s1 =>
//             for newExpr <- s1.ast.asExpr
//             yield StateType(s1, DeclType(newExpr))
//           }
//       case _ =>
//         s.meta.resolve(t.name, scope).flatMap(_.asType).map(t => StateType(s, t))
