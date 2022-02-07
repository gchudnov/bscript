package com.github.gchudnov.bscript.lang.ast
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Compile Expression to reuse functionality implemented in JVM
 *
 * The callback has a signature: Any => Either[Throwable, Any], inside of the callback there should be a pattern match and depending on the interpreter, Any is interpreted in a
 * particular way, e.g. for `InterpretVisitor`: it is `(InterpretState) => Either[Throwable, InterpretState]`
 */
final case class CompiledExpr(callback: Any => Either[Throwable, Any], retType: Type, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): CompiledExpr = copy(promoteToType = t)

object CompiledExpr:
  def apply(callback: Any => Either[Throwable, Any], retType: Type): CompiledExpr =
    new CompiledExpr(callback = callback, retType = retType, evalType = Type.Undefined, promoteToType = None)

  def apply(callback: Any => Either[Throwable, Any], retType: Type, evalType: Type): CompiledExpr =
    new CompiledExpr(callback = callback, retType = retType, evalType = evalType, promoteToType = None)

  val idCallback: Any => Either[Throwable, Any] = (s: Any) => Right(s)
