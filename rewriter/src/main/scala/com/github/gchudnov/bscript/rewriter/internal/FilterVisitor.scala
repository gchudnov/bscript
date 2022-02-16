package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }

/**
 * Filters AST-nodes that match the given predicate.
 */
private[internal] final class FilterVisitor(pred: (AST) => Boolean) extends TreeVisitor[FilterState, Option[AST]]:
  import Casting.*

  override def visit(s: FilterState, n: Init): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: UnaryMinus): Either[Throwable, Option[AST]] =
    Right(
      for
        expr <- filterAST(n.expr)
        n1    = n.copy(expr = expr)
      yield n1
    )

  override def visit(s: FilterState, n: Add): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Sub): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Mul): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Div): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Mod): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Less): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: LessEqual): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Greater): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: GreaterEqual): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Equal): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: NotEqual): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Not): Either[Throwable, Option[AST]] =
    Right(
      for
        expr <- filterAST(n.expr)
        n1    = n.copy(expr = expr)
      yield n1
    )

  override def visit(s: FilterState, n: And): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Or): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        lhs <- filterAST(n1.lhs)
        rhs <- filterAST(n1.rhs)
        n2   = n1.copy(lhs = lhs, rhs = rhs)
      yield n2
    )

  override def visit(s: FilterState, n: Assign): Either[Throwable, Option[AST]] =
    Right(
      for
        n1   <- filterAST(n)
        id   <- filterAST(n1.id)
        expr <- filterAST(n1.expr)
        n2    = n1.copy(id = id, expr = expr)
      yield n2
    )

  override def visit(s: FilterState, n: NothingVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: VoidVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: BoolVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: IntVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: LongVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: FloatVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: DoubleVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: DecimalVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: StrVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: DateVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: DateTimeVal): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: StructVal): Either[Throwable, Option[AST]] =
    Right(
      for
        n1   <- filterAST(n)
        value = filterASTMap(n1.value)
        n2    = n1.copy(value = value)
      yield n2
    )

  override def visit(s: FilterState, n: Vec): Either[Throwable, Option[AST]] =
    Right(
      for
        n1 <- filterAST(n)
        es  = n1.elements.map(el => filterAST(el)).collect { case Some(it) => it }
        n2  = n1.copy(elements = es)
      yield n2
    )

  override def visit(s: FilterState, n: Var): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: ArgDecl): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: VarDecl): Either[Throwable, Option[AST]] =
    Right(
      for
        n1   <- filterAST(n)
        expr <- filterAST(n.expr)
        n2    = n1.copy(expr = expr)
      yield n2
    )

  override def visit(s: FilterState, n: FieldDecl): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  override def visit(s: FilterState, n: MethodDecl): Either[Throwable, Option[AST]] =
    Right(
      for
        n1    <- filterAST(n)
        params = n1.params.map(p => filterAST(p)).collect { case Some(it) => it }
        body  <- filterAST(n1.body)
        n2     = n1.copy(params = params, body = body)
      yield n2
    )

  override def visit(s: FilterState, n: StructDecl): Either[Throwable, Option[AST]] =
    Right(
      for
        n1    <- filterAST(n)
        fields = n1.fields.map(f => filterAST(f)).collect { case Some(it) => it }
        n2     = n1.copy(fields = fields)
      yield n2
    )

  override def visit(s: FilterState, n: Block): Either[Throwable, Option[AST]] =
    Right(
      for
        n1        <- filterAST(n)
        statements = n1.statements.map(s => filterAST(s)).collect { case Some(it) => it }
        n2         = n1.copy(statements = statements)
      yield n2
    )

  override def visit(s: FilterState, n: Call): Either[Throwable, Option[AST]] =
    Right(
      for
        n1  <- filterAST(n)
        args = n1.args.map(s => filterAST(s)).collect { case Some(it) => it }
        n2   = n1.copy(args = args)
      yield n2
    )

  override def visit(s: FilterState, n: If): Either[Throwable, Option[AST]] =
    Right(
      for
        n1    <- filterAST(n)
        cond  <- filterAST(n1.cond)
        then1 <- filterAST(n1.then1)
        else1 <- n1.else1.map(it => filterAST(it))
        n2     = n1.copy(cond = cond, then1 = then1, else1 = else1)
      yield n2
    )

  override def visit(s: FilterState, n: Access): Either[Throwable, Option[AST]] =
    Right(
      for
        n1 <- filterAST(n)
        a  <- filterAST(n1.a)
        b  <- filterAST(n1.b)
        n2  = n1.copy(a = a, b = b)
      yield n2
    )

  override def visit(s: FilterState, n: CompiledExpr): Either[Throwable, Option[AST]] =
    Right(
      for n1 <- filterAST(n)
      yield n1
    )

  private def filterAST[T <: AST](n: T): Option[T] =
    if pred(n) then Some(n)
    else None

  // TODO: implement it:
  private def filterASTMap(m: Map[String, Expr]): Map[String, Expr] =
    ???

private[rewriter] object FilterVisitor:

  def make(pred: (AST) => Boolean): FilterVisitor =
    new FilterVisitor(pred)
