package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Filters AST-nodes that match the given predicate.
 */
private[internal] final class FilterVisitor(pred: (AST) => Boolean) extends TreeVisitor[FilterState, AST]:
  import Casting.*

  override def visit(s: FilterState, n: Init): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: UnaryMinus): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: FilterState, n: Add): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Sub): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Mul): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Div): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Mod): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Less): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: LessEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Greater): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: GreaterEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Equal): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: NotEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Not): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: FilterState, n: And): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Or): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: FilterState, n: Assign): Either[Throwable, AST] =
    for
      id   <- visitAST(n.id).flatMap(_.asLValue)
      expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(id = id, expr = expr)

  override def visit(s: FilterState, n: NothingVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: VoidVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: BoolVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: IntVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: LongVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: FloatVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: DoubleVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: DecimalVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: StrVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: DateVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: DateTimeVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: StructVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: Vec): Either[Throwable, AST] =
    for elements <- Transform.sequence(n.elements.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(elements = elements)

  override def visit(s: FilterState, n: Var): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: ArgDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: VarDecl): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: FilterState, n: FieldDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: FilterState, n: MethodDecl): Either[Throwable, AST] =
    for
      params <- Transform.sequence(n.params.map(n1 => visitAST(n1).flatMap(_.asArgDecl)))
      body   <- visitAST(n.body).flatMap(_.asBlock)
    yield n.copy(params = params, body = body)

  override def visit(s: FilterState, n: StructDecl): Either[Throwable, AST] =
    for fields <- Transform.sequence(n.fields.map(n1 => visitAST(n1).flatMap(_.asFieldDecl)))
    yield n.copy(fields = fields)

  override def visit(s: FilterState, n: Block): Either[Throwable, AST] =
    for statements <- Transform.sequence(n.statements.map(n1 => visitAST(n1).flatMap(_.asExpr))).map(_.filter(it => !it.isInstanceOf[NothingVal]))
    yield n.copy(statements = statements.toList)

  override def visit(s: FilterState, n: Call): Either[Throwable, AST] =
    for args <- Transform.sequence(n.args.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(args = args)

  override def visit(s: FilterState, n: If): Either[Throwable, AST] =
    for
      cond  <- visitAST(n.cond).flatMap(_.asExpr)
      then1 <- visitAST(n.then1).flatMap(_.asExpr)
      else1 <- Transform.sequence(n.else1.map(it => visitAST(it).flatMap(_.asExpr)))
    yield n.copy(cond = cond, then1 = then1, else1 = else1)

  override def visit(s: FilterState, n: Access): Either[Throwable, AST] =
    for
      a <- visitAST(n.a).flatMap(_.asLValue)
      b <- visitAST(n.b).flatMap(_.asLValue)
    yield n.copy(a = a, b = b)

  override def visit(s: FilterState, n: CompiledExpr): Either[Throwable, AST] =
    visitAST(n)

  private def visitAST(n: AST): Either[Throwable, AST] =
    if pred(n) then Right(n)
    else Right(NothingVal())

private[rewriter] object FilterVisitor:

  def make(pred: (AST) => Boolean): FilterVisitor =
    new FilterVisitor(pred)
