package com.github.gchudnov.bscript.lang.ast
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Compile Expression to reuse functionality implemented in JVM
 *
 * The callback has a signature: Any => Either[Throwable, Any], inside of the callback there should be a pattern match and depending on the interpreter, Any is interpreted in a
 * particular way, e.g. for `InterpretVisitor`: it is `(InterpretState) => Either[Throwable, InterpretState]`
 */
final case class CompiledExpr(callback: Any => Either[Throwable, Any], retType: Type) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

object CompiledExpr:
  val idCallback: Any => Either[Throwable, Any] = (s: Any) => Right(s)
