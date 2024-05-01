package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Maps AST-node to a different AST-node.
 */
private[internal] final class MapVisitor(f: (AST) => AST) extends TreeVisitor[MapState, AST]:
  import Casting.*

  override def visit(s: MapState, n: Init): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: UnaryMinus): Either[Throwable, AST] =
    for
      expr <- n.expr.visit(s, this).flatMap(_.asExpr)
      n1    = n.copy(expr = expr)
      n2    = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Add): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Sub): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Mul): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Div): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Mod): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Less): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: LessEqual): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Greater): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: GreaterEqual): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Equal): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: NotEqual): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Not): Either[Throwable, AST] =
    for
      expr <- n.expr.visit(s, this).flatMap(_.asExpr)
      n1    = n.copy(expr = expr)
      n2    = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: And): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Or): Either[Throwable, AST] =
    for
      lhs <- n.lhs.visit(s, this).flatMap(_.asExpr)
      rhs <- n.rhs.visit(s, this).flatMap(_.asExpr)
      n1   = n.copy(lhs = lhs, rhs = rhs)
      n2   = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Assign): Either[Throwable, AST] =
    for
      id   <- n.id.visit(s, this).flatMap(_.asLValue)
      expr <- n.expr.visit(s, this).flatMap(_.asExpr)
      n1    = n.copy(id = id, expr = expr)
      n2    = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: NothingVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: VoidVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: BoolVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: IntVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: LongVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: FloatVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: DoubleVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: DecimalVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: StrVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: DateVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: DateTimeVal): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: StructVal): Either[Throwable, AST] =
    for
      value <- visitMap(s, n.value)
      n1     = n.copy(value = value)
      n2     = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Vec): Either[Throwable, AST] =
    for
      elements <- Transform.sequence(n.elements.map(it => it.visit(s, this).flatMap(_.asExpr)))
      n1        = n.copy(elements = elements)
      n2        = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Var): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: ArgDecl): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: VarDecl): Either[Throwable, AST] =
    for
      expr <- n.expr.visit(s, this).flatMap(_.asExpr)
      n1    = n.copy(expr = expr)
      n2    = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: FieldDecl): Either[Throwable, AST] =
    Right(mapAST(n))

  override def visit(s: MapState, n: MethodDecl): Either[Throwable, AST] =
    for
      params <- Transform.sequence(n.params.map(it => it.visit(s, this).flatMap(_.asArgDecl)))
      body   <- n.body.visit(s, this).flatMap(_.asBlock)
      n1      = n.copy(params = params, body = body)
      n2      = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: StructDecl): Either[Throwable, AST] =
    for
      fields <- Transform.sequence(n.fields.map(it => it.visit(s, this).flatMap(_.asFieldDecl)))
      n1      = n.copy(fields = fields)
      n2      = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Block): Either[Throwable, AST] =
    for
      statements <- Transform.sequence(n.statements.map(it => it.visit(s, this).flatMap(_.asExpr)))
      n1          = n.copy(statements = statements.toList)
      n2          = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Module): Either[Throwable, AST] =
    for
      statements <- Transform.sequence(n.statements.map(it => it.visit(s, this).flatMap(_.asExpr)))
      n1 = n.copy(statements = statements.toList)
      n2 = mapAST(n1)
    yield n2
  
  override def visit(s: MapState, n: Call): Either[Throwable, AST] =
    for
      args <- Transform.sequence(n.args.map(it => it.visit(s, this).flatMap(_.asExpr)))
      n1    = n.copy(args = args)
      n2    = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: If): Either[Throwable, AST] =
    for
      cond  <- n.cond.visit(s, this).flatMap(_.asExpr)
      then1 <- n.then1.visit(s, this).flatMap(_.asExpr)
      else1 <- Transform.sequence(n.else1.map(it => it.visit(s, this).flatMap(_.asExpr)))
      n1     = n.copy(cond = cond, then1 = then1, else1 = else1)
      n2     = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: Access): Either[Throwable, AST] =
    for
      a <- n.a.visit(s, this).flatMap(_.asLValue)
      b <- n.b.visit(s, this).flatMap(_.asLValue)
      n1 = n.copy(a = a, b = b)
      n2 = mapAST(n1)
    yield n2

  override def visit(s: MapState, n: CompiledExpr): Either[Throwable, AST] =
    Right(mapAST(n))

  private def mapAST[T <: AST](n: T): AST =
    f(n)

  private def visitMap(s: MapState, m: Map[String, Expr]): Either[Throwable, Map[String, Expr]] =
    m.foldLeft(Right(Map.empty[String, Expr]): Either[Throwable, Map[String, Expr]]) { case (acc, (k, v)) =>
      acc match
        case Left(e) => Left(e)
        case Right(m) =>
          for v1 <- v.visit(s, this).flatMap(_.asExpr)
          yield m + (k -> v1)
    }

private[rewriter] object MapVisitor:

  def make(f: (AST) => AST): MapVisitor =
    new MapVisitor(f)
