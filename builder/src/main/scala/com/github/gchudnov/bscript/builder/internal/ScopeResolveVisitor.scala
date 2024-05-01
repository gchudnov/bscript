package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.internal.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * (2-PASS)
 *
 * The primary goal when building a symbol table is to construct a scope tree.
 *
 * We define symbols, group them into scopes, and organize those scopes into scope trees.
 *
 * Scope trees are crucial because their structure encodes the rules for looking up symbols.
 *
 * Resolving a symbol means looking for it in the current scope or any scope on the path to the root of the scope tree.
 *
 * NOTE: After this pass, there should be no Ref-symbols and Ref-types.
 *
 * ScopeBuildVisitor:
 *
 * {{{
 * 1) Pushes Scopes;
 * 2) Defines symbols in scopes;
 * }}}
 *
 * ScopeResolveVisitor:
 *
 * {{{
 * 3) Resolve Symbols (and verify that names can be referenced).
 * }}}
 *
 * Defines Actions
 *
 * Building a scope tree boils down to executing a sequence of these operations: push, pop, and def.
 *
 * [push]. At the start of a scope, push a new scope on the scope stack. This works even for complicated scopes like classes. Because we are building scope trees, push is more like
 * an “add child” tree construction operation than a conventional stack push. An implementation preview:
 *
 * {{{
 *   // create new scope whose enclosing scope is the current scope
 *   val currentScope = new LocalScope(currentScope); // push new scope
 * }}}
 *
 * [pop]. At the end of a scope, pop the current scope off the stack, revealing the previous scope as the current scope. pop moves the current scope pointer up one level in the
 * tree:
 *
 * {{{
 *   val currentScope = currentScope.getEnclosingScope(); // pop scope
 * }}}
 *
 * [def]. Define a symbol in the current scope. We’ll always define symbols like this:
 *
 * {{{
 *   val s: Symbol = «some-new-symbol»;
 *   currentScope.define(s); // define s in current scope
 * }}}
 *
 * To create a scope tree then, all we have to do is a depth-first walk of the AST, executing actions in the pre- and/or postorder position. We push as we descend and pop as we
 * ascend. When we see a symbol, we define or resolve it in the current scope.
 */
private[internal] final class ScopeResolveVisitor() extends TreeVisitor[ScopeResolveState, ScopeResolveState]:
  import Casting.*
  import ScopeResolveVisitor.*

  override def visit(s: ScopeResolveState, n: Init): Either[Throwable, ScopeResolveState] =
    for
      scope               <- s.meta.scopeFor(n)
      st                  <- visitType(s, scope, n.iType)
      StateType(s1, iType) = st
      n1                   = n.copy(iType = iType)
      ss1                  = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: UnaryMinus): Either[Throwable, ScopeResolveState] =
    for
      s1   <- n.expr.visit(s, this)
      expr <- s1.ast.asExpr
      n1    = n.copy(expr = expr)
      ss1   = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: ArgDecl): Either[Throwable, ScopeResolveState] =
    for
      scope               <- s.meta.scopeFor(n).flatMap(_.asSMethod)
      sVar                <- s.meta.resolveMember(n.name, scope).flatMap(_.asSVar)
      _                    = assert(sVar == n.symbol, "The SVar resolved is not equal to ehe Symbol stored in AST")
      st                  <- visitType(s, scope, n.aType)
      StateType(s1, aType) = st
      n1                   = n.copy(aType = aType, symbol = sVar)
      ss1                  = s1.meta.defineVarType(sVar, aType).redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: FieldDecl): Either[Throwable, ScopeResolveState] =
    for
      scope               <- s.meta.scopeFor(n).flatMap(_.asSStruct)
      sVar                <- s.meta.resolveMember(n.name, scope).flatMap(_.asSVar)
      _                    = assert(sVar == n.symbol, "The SVar resolved is not equal to ehe Symbol stored in AST")
      st                  <- visitType(s, scope, n.fType)
      StateType(s1, fType) = st
      n1                   = n.copy(fType = fType, symbol = sVar)
      ss1                  = s1.meta.defineVarType(sVar, st.xType).redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: VarDecl): Either[Throwable, ScopeResolveState] =
    for
      scope               <- s.meta.scopeFor(n)
      sVar                <- s.meta.resolveMember(n.name, scope).flatMap(_.asSVar)
      _                    = assert(sVar == n.symbol, "The SVar resolved is not equal to ehe Symbol stored in AST")
      st                  <- visitType(s, scope, n.vType)
      StateType(s1, vType) = st
      ss1                  = s1.meta.defineVarType(sVar, vType)
      s2                  <- n.expr.visit(s1.copy(meta = ss1), this)
      expr                <- s2.ast.asExpr
      n1                   = n.copy(vType = vType, expr = expr, symbol = sVar)
      ss2                  = s2.meta.redefineASTScope(n, n1)
    yield s2.copy(ast = n1, meta = ss2)

  override def visit(s: ScopeResolveState, n: MethodDecl): Either[Throwable, ScopeResolveState] =
    for
      scope <- s.meta.scopeFor(n)
      sa <- n.params.foldLeft(Right((s, List.empty[ArgDecl])): Either[Throwable, (ScopeResolveState, List[ArgDecl])]) { case (acc, argDecl) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, args)) =>
                  for
                    sy   <- argDecl.visit(sx, this)
                    argN <- sy.ast.asArgDecl
                  yield (sy, args :+ argN)
            }
      (s1, params)           = sa
      sMethod               <- s1.meta.resolveMember(n.name, scope).flatMap(_.asSMethod)
      _                      = assert(sMethod == n.symbol, "The SMethod resolved is not equal to ehe Symbol stored in AST")
      st                    <- visitType(s1, scope, n.retType)
      StateType(s2, retType) = st
      ss1                    = s2.meta.defineMethodRetType(sMethod, retType)
      s3                     = s2.copy(meta = ss1)
      s4                    <- n.body.visit(s3, this)
      body                  <- s4.ast.asBlock
      n1                     = n.copy(retType = retType, body = body, params = params, symbol = sMethod)
      ss2                    = s4.meta.defineMethodAST(sMethod, n1).redefineASTScope(n, n1)
    yield s3.copy(ast = n1, meta = ss2)

  override def visit(s: ScopeResolveState, n: StructDecl): Either[Throwable, ScopeResolveState] =
    for
      scope   <- s.meta.scopeFor(n)
      _       <- s.meta.resolve(n.name, scope).flatMap(_.asType) // NOTE: Struct is a Type as well; this line is used to assert this fact here.
      sStruct <- s.meta.resolve(n.name, scope).flatMap(_.asSStruct)
      _        = assert(sStruct == n.symbol, "The SStruct resolved is not equal to ehe Symbol stored in AST")
      sf <- n.fields.foldLeft(Right((s, List.empty[FieldDecl])): Either[Throwable, (ScopeResolveState, List[FieldDecl])]) { case (acc, fieldDecl) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, fields)) =>
                  for
                    sy     <- fieldDecl.visit(sx, this)
                    fieldN <- sy.ast.asFieldDecl
                  yield (sy, fields :+ fieldN)
            }
      (s1, fields) = sf
      n1           = n.copy(symbol = sStruct, fields = fields)
      ss1          = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Block): Either[Throwable, ScopeResolveState] =
    for
      bs <- n.statements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, exprs)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, exprs :+ exprN)
            }
      (s1, exprs) = bs
      n1          = n.copy(statements = exprs)
      ss1         = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Module): Either[Throwable, ScopeResolveState] =
    for
      bs <- n.statements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
        acc match
          case Left(ex) => Left(ex)
          case Right((sx, exprs)) =>
            for
              sy    <- expr.visit(sx, this)
              exprN <- sy.ast.asExpr
            yield (sy, exprs :+ exprN)
      }
      (s1, exprs) = bs
      n1          = n.copy(statements = exprs)
      ss1         = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Var): Either[Throwable, ScopeResolveState] =
    for
      scope <- s.meta.scopeFor(n)
      sVar  <- s.meta.resolve(n.symbol.name, scope).flatMap(_.asSVar)
      n1     = n.copy(symbol = sVar)
      ss1    = s.meta.redefineASTScope(n, n1)
    yield s.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Assign): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.id.visit(s, this)
      rs    <- n.expr.visit(ls, this)
      id1   <- ls.ast.asLValue
      expr1 <- rs.ast.asExpr
      n1     = n.copy(id = id1, expr = expr1)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: NothingVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: VoidVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: BoolVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: IntVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: LongVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: FloatVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: DoubleVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: DecimalVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: StrVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: DateVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: DateTimeVal): Either[Throwable, ScopeResolveState] =
    Right(s.copy(ast = n))

  override def visit(s: ScopeResolveState, n: StructVal): Either[Throwable, ScopeResolveState] =
    for
      scope               <- s.meta.scopeFor(n)
      st                  <- visitType(s, scope, n.sType)
      StateType(s1, sType) = st
      sv <- n.value.foldLeft(Right((s1, Map.empty[String, Expr])): Either[Throwable, (ScopeResolveState, Map[String, Expr])]) { case (acc, (name, expr)) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, map)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, map + (name -> exprN))
            }
      (s2, value1) = sv
      n1           = n.copy(sType = sType, value = value1)
      ss1          = s2.meta.redefineASTScope(n, n1)
    yield s2.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Vec): Either[Throwable, ScopeResolveState] =
    for
      se <- n.elements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, exprs)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, exprs :+ exprN)
            }
      (s1, exprs) = se
      n1          = n.copy(elements = exprs) // NOTE: element type is deduced in Phase #3
      ss1         = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Add): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Sub): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Mul): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Div): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Mod): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Less): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: LessEqual): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Greater): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: GreaterEqual): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Equal): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: NotEqual): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Not): Either[Throwable, ScopeResolveState] =
    for
      s1   <- n.expr.visit(s, this)
      expr <- s1.ast.asExpr
      n1    = n.copy(expr = expr)
      ss1   = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Return): Either[Throwable, ScopeResolveState] =
    for
      s1 <- n.expr.visit(s, this)
      expr <- s1.ast.asExpr
      n1 = n.copy(expr = expr)
      ss1 = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: And): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Or): Either[Throwable, ScopeResolveState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.redefineASTScope(n, n1)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Call): Either[Throwable, ScopeResolveState] =
    for
      scope   <- s.meta.scopeFor(n)
      sMethod <- s.meta.resolve(n.id.name, scope).flatMap(_.asSMethod)
      sa <- n.args.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeResolveState, List[Expr])]) { case (acc, expr) =>
              acc match
                case Left(ex) => Left(ex)
                case Right((sx, args)) =>
                  for
                    sy   <- expr.visit(sx, this)
                    argN <- sy.ast.asExpr
                  yield (sy, args :+ argN)
            }
      (s1, args) = sa
      n1         = n.copy(id = sMethod, args = args)
      ss1        = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: If): Either[Throwable, ScopeResolveState] =
    for
      cs          <- n.cond.visit(s, this)
      ts          <- n.then1.visit(cs, this)
      es          <- Transform.sequence(n.else1.map(_.visit(ts, this)))
      condExpr    <- cs.ast.asExpr
      thenExpr    <- ts.ast.asExpr
      optElseExpr <- Transform.sequence(es.map(_.ast.asExpr))
      s1           = es.getOrElse(ts)
      n1           = n.copy(cond = condExpr, then1 = thenExpr, else1 = optElseExpr)
      ss1          = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeResolveState, n: Access): Either[Throwable, ScopeResolveState] =

    // Find 'y'-symbol in 'x'-struct
    def member(xVar: SVar, y: Var): Either[Throwable, SVar] =
      for
        xType <- s.meta.typeFor(xVar).flatMap(_.asSStruct)
        yVar  <- s.meta.resolveMember(y.symbol.name, xType).left.map(_ => new AstException(s"Symbol '${xVar.name}.${y.symbol.name}' cannot be resolved")).flatMap(_.asSVar)
      yield yVar

    def iterate(sx: Meta, n1: Access): Either[Throwable, (Meta, (SVar, Access))] =
      (n1.a, n1.b) match
        case (x: Var, y: Var) =>
          // 'x' must be a Struct Type, 'y' is a field of struct 'x'.
          for
            scopeX <- s.meta.scopeFor(x)
            xVar   <- s.meta.resolve(x.symbol.name, scopeX).flatMap(_.asSVar)
            yVar   <- member(xVar, y)
            nx      = x.copy(symbol = xVar)
            ny      = y.copy(symbol = yVar)
            n2      = n1.copy(a = nx, b = ny)
            sy      = sx.redefineASTScope(n1, n2).redefineASTScope(x, nx).redefineASTScope(y, ny)
          yield (sy, (yVar, n2))

        case (xx: Access, y: Var) =>
          iterate(sx, xx).flatMap { case (sy, (xVar, xAcc)) =>
            for
              yVar <- member(xVar, y)
              ny    = y.copy(symbol = yVar)
              n2    = n1.copy(a = xAcc, b = ny)
              sz    = sy.redefineASTScope(n1, n2).redefineASTScope(y, ny)
            yield (sz, (yVar, n2))
          }

        case _ =>
          Left(new AstException(s"Cannot build scopes for Access('${n.a}', '${n.b}'); The expected Access structure is Access(Var, Var) or Access(Access, Var)."))

    for
      t             <- iterate(s.meta, n)
      (ss1, (_, n1)) = t
    yield s.copy(meta = ss1, ast = n1)

  override def visit(s: ScopeResolveState, n: CompiledExpr): Either[Throwable, ScopeResolveState] =
    for
      scope                 <- s.meta.scopeFor(n)
      st                    <- visitType(s, scope, n.retType)
      StateType(s1, retType) = st
      n1                     = n.copy(retType = retType)
      ss1                    = s1.meta.redefineASTScope(n, n1)
    yield s1.copy(ast = n1, meta = ss1)

  private def visitType(s: ScopeResolveState, scope: Scope, t: Type): Either[Throwable, StateType] =
    t match
      case VectorType(elementType) =>
        visitType(s, scope, elementType).map(st => st.copy(xType = VectorType(st.xType)))
      case DeclType(expr) =>
        expr
          .visit(s, this)
          .flatMap { s1 =>
            for newExpr <- s1.ast.asExpr
            yield StateType(s1, DeclType(newExpr))
          }
      case _ =>
        s.meta.resolve(t.name, scope).flatMap(_.asType).map(t => StateType(s, t))

private[builder] object ScopeResolveVisitor:

  def make(): ScopeResolveVisitor =
    new ScopeResolveVisitor()

  final case class ScopeResolveState(
    ast: AST,
    meta: Meta
  )

  object ScopeResolveState:
    def make(ast: AST, meta: Meta): ScopeResolveState =
      ScopeResolveState(
        ast = ast,
        meta = meta
      )

  private final case class StateType(
    state: ScopeResolveState,
    xType: Type
  )
