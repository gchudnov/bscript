package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.mirror.AstFolder

/**
 * (2-PASS)
 * 
 * Executed after ScopeBuildVisitor that created scopes and defined symbols in these scopes.
 *
 * ScopeResolveVisitor:
 *
 * {{{
 * 3) Resolve Symbols (and verify that names can be referenced).
 * }}}
 *
 * All we have to do is a depth-first walk of the AST, executing actions in the pre- and/or post-order position. 
 * When we see a symbol, we resolve it in the current scope.
 */
private[builder] final class ScopeResolveVisitor() extends AstFolder[ScopeResolver]:
  
  override def foldAST(a: ScopeResolver, ast: AST): ScopeResolver =
    ast match
      case x: Access =>
        // foldOverTree(a, x)
        ???
      case x @ ArgDecl(aType, name) =>
        //foldOverTree(a.define(SVar(name)), x)
        ???
      // case x: Assign =>
      //   foldOverTree(a, x)
      case x: Block =>
        foldOverTree(a, x)
      // case x @ Literal(_) =>
      //   foldOverTree(a, x)
      // case x @ Call(id, args) =>
      //   foldOverTree(a.bind(x), x)
      // case x @ Compiled(_, retType) =>
      //   foldOverTree(a, x)
      // case x @ FieldDecl(fType, name) =>
      //   foldOverTree(a.define(SVar(name)), x)
      // case x: If =>
      //   foldOverTree(a, x)
      case x @ Init(iType) =>
        foldOverTree(a, x)
      // case x @ MethodDecl(retType, name, _, _) =>
      //   foldOverTree(a.define(SMethod(name)).push(), x).pop()
      // case x @ StructDecl(name, _) =>
      //   foldOverTree(a.define(SStruct(name)).push(), x).pop()
      // case x @ Var(sym) =>
      //   foldOverTree(a, x)
      case x @ VarDecl(vType, name, _) =>
        foldOverTree(a.resolveVarDecl(name, vType, x), x)
      // case x @ Vec(_, elementType) =>
      //   foldOverTree(a, x)

//   override def visit(s: ScopeResolveState, n: ArgDecl): Either[Throwable, ScopeResolveState] =
//     for
//       scope               <- s.meta.scopeFor(n).flatMap(_.asSMethod)
//       sVar                <- s.meta.resolveMember(n.name, scope).flatMap(_.asSVar)
//       st                  <- visitType(s, scope, n.aType)
//       StateType(s1, aType) = st
//       n1                   = n.copy(aType = aType)
//       ss1                  = s1.meta.defineVarType(sVar, aType).redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)


private[builder] object ScopeResolveVisitor:

  def make(): ScopeResolveVisitor =
    new ScopeResolveVisitor()


//   override def visit(s: ScopeResolveState, n: Access): Either[Throwable, ScopeResolveState] =

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

//   override def visit(s: ScopeResolveState, n: FieldDecl): Either[Throwable, ScopeResolveState] =
//     for
//       scope               <- s.meta.scopeFor(n).flatMap(_.asSStruct)
//       sVar                <- s.meta.resolveMember(n.name, scope).flatMap(_.asSVar)
//       st                  <- visitType(s, scope, n.fType)
//       StateType(s1, fType) = st
//       n1                   = n.copy(fType = fType)
//       ss1                  = s1.meta.defineVarType(sVar, st.xType).redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   override def visit(s: ScopeResolveState, n: MethodDecl): Either[Throwable, ScopeResolveState] =
//     for
//       scope <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       sa <- n.params.foldLeft(Right((s, List.empty[ArgDecl])): Either[Throwable, (ScopeResolveState, List[ArgDecl])]) { case (acc, argDecl) =>
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

//   override def visit(s: ScopeResolveState, n: StructDecl): Either[Throwable, ScopeResolveState] =
//     for
//       scope   <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       _       <- s.meta.resolve(n.name, scope).flatMap(_.asType) // NOTE: Struct is a Type as well; this line is used to assert this fact here.
//       sStruct <- s.meta.resolve(n.name, scope).flatMap(_.asSStruct)
//       sf <- n.fields.foldLeft(Right((s, List.empty[FieldDecl])): Either[Throwable, (ScopeResolveState, List[FieldDecl])]) { case (acc, fieldDecl) =>
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


//   override def visit(s: ScopeResolveState, n: Var): Either[Throwable, ScopeResolveState] =
//     for
//       scope <- s.meta.scopeFor(n)
//       sVar  <- s.meta.resolve(n.symbol.name, scope).flatMap(_.asSVar)
//       n1     = n.copy(symbol = sVar)
//       ss1    = s.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s.copy(ast = n1, meta = ss1)

//   override def visit(s: ScopeResolveState, n: Assign): Either[Throwable, ScopeResolveState] =
//     for
//       ls    <- n.id.visit(s, this)
//       rs    <- n.expr.visit(ls, this)
//       id1   <- ls.ast.asLValue
//       expr1 <- rs.ast.asExpr
//       n1     = n.copy(id = id1, expr = expr1)
//       ss1    = rs.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield rs.copy(ast = n1, meta = ss1)

//   override def visit(s: ScopeResolveState, n: StructVal): Either[Throwable, ScopeResolveState] =
//     for
//       scope               <- s.meta.scopeFor(n)
//       st                  <- visitType(s, scope, n.sType)
//       StateType(s1, sType) = st
//       sv <- n.value.foldLeft(Right((s1, Map.empty[String, Expr])): Either[Throwable, (ScopeResolveState, Map[String, Expr])]) { case (acc, (name, expr)) =>
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

//   override def visit(s: ScopeResolveState, n: Vec): Either[Throwable, ScopeResolveState] =
//     for
//       se <- n.elements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
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

//   override def visit(s: ScopeResolveState, n: Call): Either[Throwable, ScopeResolveState] =
//     for
//       scope   <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       sMethod <- s.meta.resolve(n.id.name, scope).flatMap(_.asSMethod)
//       sa <- n.args.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
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

//   override def visit(s: ScopeResolveState, n: If): Either[Throwable, ScopeResolveState] =
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

//   override def visit(s: ScopeResolveState, n: CompiledExpr): Either[Throwable, ScopeResolveState] =
//     for
//       scope                 <- s.meta.scopeFor(n).flatMap(_.asSBlock)
//       st                    <- visitType(s, scope, n.retType)
//       StateType(s1, retType) = st
//       n1                     = n.copy(retType = retType)
//       ss1                    = s1.meta.redefineASTScope(n, n1).ensureNoAST(n)
//     yield s1.copy(ast = n1, meta = ss1)

//   private def visitType(s: ScopeResolveState, scope: Scope, t: Type): Either[Throwable, StateType] =
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
