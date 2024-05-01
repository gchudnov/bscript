package com.github.gchudnov.bscript.rewriter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }
import scala.collection.immutable.Seq

/**
 * Finds the first AST-node that matches the given predicate.
 */
private[internal] final class FindVisitor(pred: (AST) => Boolean) extends TreeVisitor[FindState, Option[AST]]:
  import Casting.*

  override def visit(s: FindState, n: Init): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  def foundOrElse[T <: AST](value: Option[T])(alternative: => Either[Throwable, Option[AST]]): Either[Throwable, Option[AST]] =
    value match
      case Some(v) => Right(Some(v))
      case None    => alternative

  override def visit(s: FindState, n: UnaryMinus): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.expr.visit(s, this))
    yield n2

  override def visit(s: FindState, n: Add): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Sub): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Mul): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Div): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Mod): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Less): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: LessEqual): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Greater): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: GreaterEqual): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Equal): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: NotEqual): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Not): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.expr.visit(s, this))
    yield n2

  override def visit(s: FindState, n: And): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Or): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.lhs.visit(s, this))
      n3 <- foundOrElse(n2)(n.rhs.visit(s, this))
    yield n3

  override def visit(s: FindState, n: Assign): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.id.visit(s, this))
      n3 <- foundOrElse(n2)(n.expr.visit(s, this))
    yield n3

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
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitMap(s, n.value))
    yield n2

  override def visit(s: FindState, n: Vec): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitSeq(s, n.elements))
    yield n2

  override def visit(s: FindState, n: Var): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: ArgDecl): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: VarDecl): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.expr.visit(s, this))
    yield n2

  override def visit(s: FindState, n: FieldDecl): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  override def visit(s: FindState, n: MethodDecl): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.body.visit(s, this))
      n3 <- foundOrElse(n2)(visitSeq(s, n.params))
    yield n3

  override def visit(s: FindState, n: StructDecl): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitSeq(s, n.fields))
    yield n2

  override def visit(s: FindState, n: Block): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitSeq(s, n.statements))
    yield n2

  override def visit(s: FindState, n: Module): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitSeq(s, n.statements))
    yield n2
  
  override def visit(s: FindState, n: Call): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(visitSeq(s, n.args))
    yield n2

  override def visit(s: FindState, n: If): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.cond.visit(s, this))
      n3 <- foundOrElse(n2)(n.then1.visit(s, this))
      n4 <- foundOrElse(n3)(n.else1.fold(Right(n3))(_.visit(s, this)))
    yield n4

  override def visit(s: FindState, n: Access): Either[Throwable, Option[AST]] =
    for
      n1 <- Right(findAST(n))
      n2 <- foundOrElse(n1)(n.a.visit(s, this))
      n3 <- foundOrElse(n2)(n.b.visit(s, this))
    yield n3

  override def visit(s: FindState, n: CompiledExpr): Either[Throwable, Option[AST]] =
    Right(findAST(n))

  private def findAST[T <: AST](n: T): Option[T] =
    if pred(n) then Some(n)
    else None

  private def visitMap(s: FindState, m: Map[String, Expr]): Either[Throwable, Option[AST]] =
    m.foldLeft((Right(None): Either[Throwable, Option[AST]])) { case (acc, (k, v)) =>
      acc match
        case Left(e) =>
          Left(e)
        case Right(Some(n)) =>
          Right(Some(n))
        case Right(None) =>
          v.visit(s, this)
    }

  private def visitSeq(s: FindState, ns: Seq[Expr]): Either[Throwable, Option[AST]] =
    ns.foldLeft(Right(None): Either[Throwable, Option[AST]]) { case (acc, n) =>
      acc match
        case Left(e) =>
          Left(e)
        case Right(Some(n)) =>
          Right(Some(n))
        case Right(None) =>
          n.visit(s, this)
    }

private[rewriter] object FindVisitor:

  def make(pred: (AST) => Boolean): FindVisitor =
    new FindVisitor(pred)
