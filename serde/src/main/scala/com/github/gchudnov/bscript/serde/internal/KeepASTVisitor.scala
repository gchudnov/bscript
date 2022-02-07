package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.serde.internal.KeepASTVisitor.KeepPredicate
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.lang.types.VisitorOps.*

/**
 * Removes AST-nodes that are matching the given predicate. Used to remove [std] methods before serialization.
 */
private[internal] final class KeepASTVisitor(pred: KeepPredicate) extends TreeVisitor[Unit, AST]:

  override def visit(s: Unit, n: Init): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: UnaryMinus): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: Unit, n: Add): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Sub): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Mul): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Div): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Mod): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Less): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: LessEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Greater): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: GreaterEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Equal): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: NotEqual): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Not): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: Unit, n: And): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Or): Either[Throwable, AST] =
    for
      lhs <- visitAST(n.lhs).flatMap(_.asExpr)
      rhs <- visitAST(n.rhs).flatMap(_.asExpr)
    yield n.copy(lhs = lhs, rhs = rhs)

  override def visit(s: Unit, n: Assign): Either[Throwable, AST] =
    for
      id   <- visitAST(n.id).flatMap(_.asLValue)
      expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(id = id, expr = expr)

  override def visit(s: Unit, n: NothingVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: VoidVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: BoolVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: IntVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: LongVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: FloatVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: DoubleVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: DecimalVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: StrVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: DateVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: DateTimeVal): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: Vec): Either[Throwable, AST] =
    for elements <- Transform.sequence(n.elements.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(elements = elements)

  override def visit(s: Unit, n: Var): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: ArgDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: VarDecl): Either[Throwable, AST] =
    for expr <- visitAST(n.expr).flatMap(_.asExpr)
    yield n.copy(expr = expr)

  override def visit(s: Unit, n: FieldDecl): Either[Throwable, AST] =
    visitAST(n)

  override def visit(s: Unit, n: MethodDecl): Either[Throwable, AST] =
    for
      params <- Transform.sequence(n.params.map(n1 => visitAST(n1).flatMap(_.asArgDecl)))
      body   <- visitAST(n.body).flatMap(_.asBlock)
    yield n.copy(params = params, body = body)

  override def visit(s: Unit, n: StructDecl): Either[Throwable, AST] =
    for fields <- Transform.sequence(n.fields.map(n1 => visitAST(n1).flatMap(_.asFieldDecl)))
    yield n.copy(fields = fields)

  override def visit(s: Unit, n: Block): Either[Throwable, AST] =
    for statements <- Transform.sequence(n.statements.map(n1 => visitAST(n1).flatMap(_.asExpr))).map(_.filter(it => !it.isInstanceOf[NothingVal]))
    yield n.copy(statements = statements.toList)

  override def visit(s: Unit, n: Call): Either[Throwable, AST] =
    for args <- Transform.sequence(n.args.map(n1 => visitAST(n1).flatMap(_.asExpr)))
    yield n.copy(args = args)

  override def visit(s: Unit, n: If): Either[Throwable, AST] =
    for
      cond  <- visitAST(n.cond).flatMap(_.asExpr)
      then1 <- visitAST(n.then1).flatMap(_.asExpr)
      else1 <- Transform.sequence(n.else1.map(it => visitAST(it).flatMap(_.asExpr)))
    yield n.copy(cond = cond, then1 = then1, else1 = else1)

  override def visit(s: Unit, n: Access): Either[Throwable, AST] =
    for
      a <- visitAST(n.a).flatMap(_.asLValue)
      b <- visitAST(n.b).flatMap(_.asLValue)
    yield n.copy(a = a, b = b)

  override def visit(s: Unit, n: CompiledExpr): Either[Throwable, AST] =
    visitAST(n)

  private def visitAST(n: AST): Either[Throwable, AST] =
    Right(pred(n) match
      case true  => n
      case false => NothingVal()
    )

private[internal] object KeepASTVisitor:

  type KeepPredicate = (AST) => Boolean

  def hasStdAnn(n: AST): Boolean =
    n match
      case m: MethodDecl => m.annotations.exists(_.isInstanceOf[StdAnn])
      case _             => false

  def hasNoStdAnn(n: AST): Boolean =
    !hasStdAnn(n)

  def make(pred: KeepPredicate): KeepASTVisitor =
    new KeepASTVisitor(pred)
