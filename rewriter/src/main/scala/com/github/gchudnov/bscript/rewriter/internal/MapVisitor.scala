package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Maps AST-node to a different AST-node.
 */
private[internal] final class MapVisitor(f: (AST) => AST) extends TreeVisitor[MapState, AST] {
  import Casting.*

  override def visit(s: MapState, n: Init): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: UnaryMinus): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: MapState, n: Add): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Sub): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Mul): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Div): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Mod): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Less): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: LessEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Greater): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: GreaterEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Equal): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: NotEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Not): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: MapState, n: And): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Or): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: MapState, n: Assign): Either[Throwable, AST] =
    for
      id   <- visitAST(n.id).flatMap(_.asLValue)
      expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(id = id, expr = expr)

  override def visit(s: MapState, n: NothingVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: VoidVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: BoolVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: IntVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: LongVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: FloatVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: DoubleVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: DecimalVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: StrVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: DateVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: DateTimeVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: StructVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: Vec): Either[Throwable, AST] =
    for elements <- Transform.sequence(n.elements.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(elements = elements)

  override def visit(s: MapState, n: Var): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: ArgDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: VarDecl): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: MapState, n: FieldDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: MapState, n: MethodDecl): Either[Throwable, AST] =
    for
      params <- Transform.sequence(n.params.map(n1 => visitAST(n1).flatMap(_.asArgDecl)))
      body   <- visitAST(n.body).flatMap(_.asBlock)
    yield n.copy(params = params, body = body)

  override def visit(s: MapState, n: StructDecl): Either[Throwable, AST] =
    for fields <- Transform.sequence(n.fields.map(n1 => visitAST(n1).flatMap(_.asFieldDecl)))
    yield n.copy(fields = fields)

  override def visit(s: MapState, n: Block): Either[Throwable, AST] =
    for statements <- Transform.sequence(n.statements.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(statements = statements.toList)

  override def visit(s: MapState, n: Call): Either[Throwable, AST] =
    for args <- Transform.sequence(n.args.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(args = args)

  override def visit(s: MapState, n: If): Either[Throwable, AST] =
    for
      cond  <- visitAST(n.cond).flatMap(_.asExpr)
      then1 <- visitAST(n.then1).flatMap(_.asExpr)
      else1 <- Transform.sequence(n.else1.map(it => visitAST(it).flatMap(_.asExpr)))
    yield n.copy(cond = cond, then1 = then1, else1 = else1)

  override def visit(s: MapState, n: Access): Either[Throwable, AST] =
    for
      a <- visitAST(n.a).flatMap(_.asLValue)
      b <- visitAST(n.b).flatMap(_.asLValue)
    yield n.copy(a = a, b = b)

  override def visit(s: MapState, n: CompiledExpr): Either[Throwable, AST] =
    visitAST(n)

  private def visitAST(n: AST): Either[Throwable, AST] =
    Right(f(n))

}


private[rewriter] object MapVisitor:

  def make(f: (AST) => AST): MapVisitor =
    new MapVisitor(f)
