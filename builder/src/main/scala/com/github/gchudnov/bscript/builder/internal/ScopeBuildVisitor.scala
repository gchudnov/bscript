package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor.{ ScopeBuildState, StateType }
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * (1-PASS)
 *
 * The primary goal when building a symbol table is to construct a scope tree.
 *
 * We define symbols, group them into scopes, and organize those scopes into scope trees.
 *
 * Scope trees are crucial because their structure encodes the rules for looking up symbols.
 *
 * Resolving a symbol means looking for it in the current scope or any scope on the path to the root of the scope tree.
 *
 * At this pass, symbols should be defined in scopes.
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
 *
 * NOTE: in the {{{ScopeBuildState]}}} it is *very* important that *different* instances of the same case class with the same value are different. It affects symbol resolution.
 */
private[internal] final class ScopeBuildVisitor() extends TreeVisitor[ScopeBuildState, ScopeBuildState]:
  import Casting.*

  override def visit(s: ScopeBuildState, n: Init): Either[Throwable, ScopeBuildState] =
    for
      st                  <- visitType(s, n.iType)
      StateType(s1, iType) = st
      n1                   = n.copy(iType = iType)
      ss1                  = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: UnaryMinus): Either[Throwable, ScopeBuildState] =
    for
      s1   <- n.expr.visit(s, this)
      expr <- s1.ast.asExpr
      n1    = n.copy(expr = expr)
      ss1   = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: ArgDecl): Either[Throwable, ScopeBuildState] =
    for
      sMethod             <- s.curScope.asSMethod
      st                  <- visitType(s, n.aType)
      StateType(s1, aType) = st
      sArg                 = SVar(n.name)
      n1                   = n.copy(aType = aType, symbol = sArg)
      ss1                  = s1.meta.defineASTScope(n1, s1.curScope).defineMethodArg(sMethod, sArg)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: FieldDecl): Either[Throwable, ScopeBuildState] =
    for
      sStruct             <- s.curScope.asSStruct
      st                  <- visitType(s, n.fType)
      StateType(s1, fType) = st
      sField               = SVar(n.name)
      n1                   = n.copy(fType = fType, symbol = sField)
      ss1                  = s1.meta.defineASTScope(n1, s1.curScope).defineStructField(sStruct, sField)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: VarDecl): Either[Throwable, ScopeBuildState] =
    for
      sScope              <- Right(s.curScope)
      st                  <- visitType(s, n.vType)
      StateType(s1, vType) = st
      s2                  <- n.expr.visit(s1, this)
      expr                <- s2.ast.asExpr
      sVar                 = SVar(n.name)
      n1                   = n.copy(vType = vType, expr = expr, symbol = sVar)
      ss1                  = s2.meta.defineASTScope(n1, s2.curScope).defineVar(sVar, sScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: MethodDecl): Either[Throwable, ScopeBuildState] =
    for
      sScope <- Right(s.curScope)
      sMethod = SMethod(n.name)
      ss1     = s.meta.defineMethod(sMethod, sScope)
      s1      = s.copy(meta = ss1, curScope = sMethod)
      sa <- n.params.foldLeft(Right((s1, List.empty[ArgDecl])): Either[Throwable, (ScopeBuildState, List[ArgDecl])]) { (acc, argDecl) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, args)) =>
                  for
                    sy   <- argDecl.visit(sx, this)
                    argN <- sy.ast.asArgDecl
                  yield (sy, args :+ argN)
            }
      (s2, args)             = sa
      st                    <- visitType(s2, n.retType)
      StateType(s3, retType) = st
      s4                    <- n.body.visit(s3, this)
      body                  <- s4.ast.asBlock
      n1                     = n.copy(retType = retType, body = body, symbol = sMethod, params = args)
      ss2                    = s4.meta.defineASTScope(n1, s.curScope)
    yield s4.copy(ast = n1, meta = ss2, curScope = s.curScope)

  override def visit(s: ScopeBuildState, n: StructDecl): Either[Throwable, ScopeBuildState] =
    for
      sScope <- Right(s.curScope) // SBlock | SModule
      sStruct = SStruct(n.name)
      ss1     = s.meta.defineStruct(sStruct, sScope)
      s1      = s.copy(meta = ss1, curScope = sStruct)
      sf <- n.fields.foldLeft(Right((s1, List.empty[FieldDecl])): Either[Throwable, (ScopeBuildState, List[FieldDecl])]) { (acc, fieldDecl) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, fields)) =>
                  for
                    sy     <- fieldDecl.visit(sx, this)
                    fieldN <- sy.ast.asFieldDecl
                  yield (sy, fields :+ fieldN)
            }
      (s2, fields) = sf
      n1           = n.copy(symbol = sStruct, fields = fields)
      ss2          = s2.meta.defineASTScope(n1, s.curScope)
    yield s2.copy(ast = n1, meta = ss2, curScope = s.curScope)

  override def visit(s: ScopeBuildState, n: Block): Either[Throwable, ScopeBuildState] =
    val (gen1, blockName) = s.gen.name()
    val sBlock            = SBlock(s"#${blockName}")
    val ss1               = s.meta.defineBlock(sBlock, s.curScope)
    val s1                = s.copy(meta = ss1, curScope = sBlock, gen = gen1)
    for
      ss <- n.statements.foldLeft(Right((s1, List.empty[Expr])): Either[Throwable, (ScopeBuildState, List[Expr])]) { (acc, expr) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, exprs)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, exprs :+ exprN)

            }
      (s2, exprs) = ss
      n1          = n.copy(statements = exprs, symbol = sBlock)
      ss2         = s2.meta.defineASTScope(n1, s.curScope)
    yield s2.copy(ast = n1, meta = ss2, curScope = s.curScope)

  override def visit(s: ScopeBuildState, n: Module): Either[Throwable, ScopeBuildState] =
    val (gen1, moduleName) = s.gen.name()
    val sModule = SModule(s"#${moduleName}")
    val ss1 = s.meta.defineModule(sModule, s.curScope)
    val s1 = s.copy(meta = ss1, curScope = sModule, gen = gen1)
    for
      ss <- n.statements.foldLeft(Right((s1, List.empty[Expr])): Either[Throwable, (ScopeBuildState, List[Expr])]) { (acc, expr) =>
        acc match
          case Left(t) => Left(t)
          case Right((sx, exprs)) =>
            for
              sy <- expr.visit(sx, this)
              exprN <- sy.ast.asExpr
            yield (sy, exprs :+ exprN)

      }
      (s2, exprs) = ss
      n1 = n.copy(statements = exprs, symbol = sModule)
      ss2 = s2.meta.defineASTScope(n1, s.curScope)
    yield s2.copy(ast = n1, meta = ss2, curScope = s.curScope)

  override def visit(s: ScopeBuildState, n: Var): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: Assign): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.id.visit(s, this)
      rs    <- n.expr.visit(ls, this)
      id1   <- ls.ast.asLValue
      expr1 <- rs.ast.asExpr
      n1     = n.copy(id = id1, expr = expr1)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: NothingVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: VoidVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: BoolVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: IntVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: LongVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: FloatVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: DoubleVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: DecimalVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: StrVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: DateVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: DateTimeVal): Either[Throwable, ScopeBuildState] =
    for ss1 <- Right(s.meta.defineASTScope(n, s.curScope))
    yield s.copy(ast = n, meta = ss1)

  override def visit(s: ScopeBuildState, n: StructVal): Either[Throwable, ScopeBuildState] =
    val (gen1, anonName) = s.gen.name()
    val sStruct          = SStruct(s"${n.sType.name}#${anonName}") // NOTE: anonymous struct; NOTE: at the moment we're not defining fields in the anonymous struct, define them ?
    for
      st                  <- visitType(s, n.sType)
      StateType(s1, sType) = st
      ss1                  = s1.meta.defineStruct(sStruct, s1.curScope)
      s2                   = s1.copy(meta = ss1, curScope = sStruct)
      sv <- n.value.foldLeft(Right((s2, Map.empty[String, Expr])): Either[Throwable, (ScopeBuildState, Map[String, Expr])]) { case (acc, (name, expr)) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, map)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, map + (name -> exprN))
            }
      (s3, value1) = sv
      n1           = n.copy(sType = sType, value = value1, symbol = sStruct)
      ss2          = s3.meta.defineASTScope(n1, s.curScope)
    yield s3.copy(ast = n1, meta = ss2, curScope = s.curScope, gen = gen1)

  override def visit(s: ScopeBuildState, n: Vec): Either[Throwable, ScopeBuildState] =
    for
      se <- n.elements.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeBuildState, List[Expr])]) { (acc, expr) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, exprs)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, exprs :+ exprN)
            }
      (s1, exprs) = se
      n1          = n.copy(elements = exprs)
      ss1         = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Add): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Sub): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Mul): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Div): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Mod): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Less): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: LessEqual): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Greater): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: GreaterEqual): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Equal): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: NotEqual): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Not): Either[Throwable, ScopeBuildState] =
    for
      s1   <- n.expr.visit(s, this)
      expr <- s1.ast.asExpr
      n1    = n.copy(expr = expr)
      ss1   = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: And): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Or): Either[Throwable, ScopeBuildState] =
    for
      ls    <- n.lhs.visit(s, this)
      rs    <- n.rhs.visit(ls, this)
      lExpr <- ls.ast.asExpr
      rExpr <- rs.ast.asExpr
      n1     = n.copy(lhs = lExpr, rhs = rExpr)
      ss1    = rs.meta.defineASTScope(n1, rs.curScope)
    yield rs.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Call): Either[Throwable, ScopeBuildState] =
    for
      sa <- n.args.foldLeft(Right((s, List.empty[Expr])): Either[Throwable, (ScopeBuildState, List[Expr])]) { (acc, expr) =>
              acc match
                case Left(t) => Left(t)
                case Right((sx, exprs)) =>
                  for
                    sy    <- expr.visit(sx, this)
                    exprN <- sy.ast.asExpr
                  yield (sy, exprs :+ exprN)
            }
      (s1, exprs) = sa
      n1          = n.copy(args = exprs)
      ss1         = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: If): Either[Throwable, ScopeBuildState] =
    for
      cs          <- n.cond.visit(s, this)
      ts          <- n.then1.visit(cs, this)
      es          <- Transform.sequence(n.else1.map(_.visit(ts, this)))
      condExpr    <- cs.ast.asExpr
      thenExpr    <- ts.ast.asExpr
      optElseExpr <- Transform.sequence(es.map(_.ast.asExpr))
      s1           = es.getOrElse(ts)
      n1           = n.copy(cond = condExpr, then1 = thenExpr, else1 = optElseExpr)
      ss1          = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: Access): Either[Throwable, ScopeBuildState] =
    for
      sa    <- n.a.visit(s, this)
      sb    <- n.b.visit(sa, this)
      aExpr <- sa.ast.asLValue
      bExpr <- sb.ast.asLValue
      n1     = n.copy(a = aExpr, b = bExpr)
      ss1    = sb.meta.defineASTScope(n1, sb.curScope)
    yield sb.copy(ast = n1, meta = ss1)

  override def visit(s: ScopeBuildState, n: CompiledExpr): Either[Throwable, ScopeBuildState] =
    for
      st                    <- visitType(s, n.retType)
      StateType(s1, retType) = st
      n1                     = n.copy(retType = retType)
      ss1                    = s1.meta.defineASTScope(n1, s1.curScope)
    yield s1.copy(ast = n1, meta = ss1)

  private def visitType(s: ScopeBuildState, t: Type): Either[Throwable, StateType] =
    t match
      case VectorType(elementType) =>
        visitType(s, elementType).map(st => st.copy(xType = VectorType(st.xType)))
      case DeclType(expr) =>
        expr
          .visit(s, this)
          .flatMap { s1 =>
            for newExpr <- s1.ast.asExpr
            yield StateType(s1, DeclType(newExpr))
          }
      case _ =>
        Right(StateType(s, t))

private[builder] object ScopeBuildVisitor:

  def make(): ScopeBuildVisitor =
    new ScopeBuildVisitor()

  final case class ScopeBuildState(
    ast: AST,
    meta: Meta,
    curScope: Scope,
    gen: Gen
  )

  object ScopeBuildState:
    def make(ast: AST, initState: Meta, rootScope: Scope, gen: Gen): ScopeBuildState =
      ScopeBuildState(
        ast = ast,
        meta = initState,
        curScope = rootScope,
        gen = gen
      )

  private final case class StateType(
    state: ScopeBuildState,
    xType: Type
  )
