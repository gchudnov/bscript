package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Finds the first AST-node that matches the given predicate.
 */
private[internal] final class FindVisitor(pred: (AST) => Boolean) extends TreeVisitor[FindState, Option[AST]]:
  import Casting.*

  override def visit(s: FindState, n: Init): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  // TODO: impl it

  override def visit(s: FindState, n: UnaryMinus): Either[Throwable, Option[AST]] =
    for
      oExpr <- n.expr.visit(s, this).flatMap(toExpr)
      nn = (for
             expr <- oExpr
             n1    = n.copy(expr = expr)
             n2   <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Add): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Sub): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Mul): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Div): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Mod): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Less): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: LessEqual): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Greater): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: GreaterEqual): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Equal): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: NotEqual): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Not): Either[Throwable, Option[AST]] =
    for
      oExpr <- n.expr.visit(s, this).flatMap(toExpr)
      nn = (for
             expr <- oExpr
             n1    = n.copy(expr = expr)
             n2   <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: And): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Or): Either[Throwable, Option[AST]] =
    for
      lExpr <- n.lhs.visit(s, this).flatMap(toExpr)
      rExpr <- n.rhs.visit(s, this).flatMap(toExpr)
      nn = (for
             lhs <- lExpr
             rhs <- rExpr
             n1   = n.copy(lhs = lhs, rhs = rhs)
             n2  <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Assign): Either[Throwable, Option[AST]] =
    for
      oId   <- n.id.visit(s, this).flatMap(toLValue)
      oExpr <- n.expr.visit(s, this).flatMap(toExpr)
      nn = (for
             id   <- oId
             expr <- oExpr
             n1    = n.copy(id = id, expr = expr)
             n2   <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: NothingVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: VoidVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: BoolVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: IntVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: LongVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: FloatVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: DoubleVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: DecimalVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: StrVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: DateVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: DateTimeVal): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: StructVal): Either[Throwable, Option[AST]] =
    for
      value <- visitMap(s, n.value)
      n1     = n.copy(value = value)
      n2     = findAST(n1)
    yield n2

  override def visit(s: FindState, n: Vec): Either[Throwable, Option[AST]] =
    for
      elements <- Transform.sequence(n.elements.map(it => it.visit(s, this).flatMap(toExpr))).map(_.collect { case Some(it) => it })
      n1        = n.copy(elements = elements)
      n2        = findAST(n1)
    yield n2

  override def visit(s: FindState, n: Var): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: ArgDecl): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: VarDecl): Either[Throwable, Option[AST]] =
    for
      oExpr <- n.expr.visit(s, this).flatMap(toExpr)
      nn = (for
             expr <- oExpr
             n1    = n.copy(expr = expr)
             n2   <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: FieldDecl): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: MethodDecl): Either[Throwable, Option[AST]] =
    for
      params <- Transform.sequence(n.params.map(it => it.visit(s, this).flatMap(toArgDecl))).map(_.collect { case Some(it) => it })
      oBody  <- n.body.visit(s, this).flatMap(toBlock)
      nn = (for
             body <- oBody
             n1    = n.copy(params = params, body = body)
             n2   <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: StructDecl): Either[Throwable, Option[AST]] =
    for
      fields <- Transform.sequence(n.fields.map(it => it.visit(s, this).flatMap(toFieldDecl))).map(_.collect { case Some(it) => it })
      n1      = n.copy(fields = fields)
      n2      = findAST(n1)
    yield n2

  override def visit(s: FindState, n: Block): Either[Throwable, Option[AST]] =
    for
      statements <- Transform.sequence(n.statements.map(it => it.visit(s, this).flatMap(toExpr))).map(_.collect { case Some(it) => it })
      n1          = n.copy(statements = statements.toList)
      n2          = findAST(n1)
    yield n2

  override def visit(s: FindState, n: Call): Either[Throwable, Option[AST]] =
    for
      args <- Transform.sequence(n.args.map(it => it.visit(s, this).flatMap(toExpr))).map(_.collect { case Some(it) => it })
      n1    = n.copy(args = args)
      n2    = findAST(n1)
    yield n2

  override def visit(s: FindState, n: If): Either[Throwable, Option[AST]] =
    for
      oCond <- n.cond.visit(s, this).flatMap(toExpr)
      oThen <- n.then1.visit(s, this).flatMap(toExpr)
      oElse <- Transform.sequence(n.else1.map(it => it.visit(s, this).flatMap(toExpr))).map(_.flatten)
      nn = (for
             cond  <- oCond
             then1 <- oThen
             n1     = n.copy(cond = cond, then1 = then1, else1 = oElse)
             n2    <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: Access): Either[Throwable, Option[AST]] =
    for
      oA <- n.a.visit(s, this).flatMap(toLValue)
      oB <- n.b.visit(s, this).flatMap(toLValue)
      nn = (for
             a  <- oA
             b  <- oB
             n1  = n.copy(a = a, b = b)
             n2 <- findAST(n1)
           yield n2)
    yield nn

  override def visit(s: FindState, n: CompiledExpr): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  private def findAST[T <: AST](n: T): Option[T] =
    if pred(n) then Some(n)
    else None

  private def visitMap(s: FindState, m: Map[String, Expr]): Either[Throwable, Map[String, Expr]] =
    m.foldLeft(Right(Map.empty[String, Expr]): Either[Throwable, Map[String, Expr]]) { case (acc, (k, v)) =>
      acc match
        case Left(e) => Left(e)
        case Right(m) =>
          for
            v1 <- v.visit(s, this)
            v2 <- Transform.sequence(v1.map(_.asExpr))
            m1 = v2 match
                   case Some(it) =>
                     m + (k -> it)
                   case None =>
                     m
          yield m1
    }

  private def toExpr(n: Option[AST]): Either[Throwable, Option[Expr]] =
    Transform.sequence(n.map(_.asExpr))

  private def toLValue(n: Option[AST]): Either[Throwable, Option[LValue]] =
    Transform.sequence(n.map(_.asLValue))

  private def toArgDecl(n: Option[AST]): Either[Throwable, Option[ArgDecl]] =
    Transform.sequence(n.map(_.asArgDecl))

  private def toFieldDecl(n: Option[AST]): Either[Throwable, Option[FieldDecl]] =
    Transform.sequence(n.map(_.asFieldDecl))

  private def toBlock(n: Option[AST]): Either[Throwable, Option[Block]] =
    Transform.sequence(n.map(_.asBlock))

private[rewriter] object FindVisitor:

  def make(pred: (AST) => Boolean): FindVisitor =
    new FindVisitor(pred)
