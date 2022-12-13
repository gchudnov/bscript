package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Compile Expression to reuse functionality implemented in JVM
 *
 * The callback has a signature: Any => Either[Throwable, Any], inside of the callback there should be a pattern match and depending on the interpreter, Any is interpreted in a
 * particular way, e.g. for `InterpretVisitor`: it is `(InterpretState) => Either[Throwable, InterpretState]`
 */
final case class Compiled(callback: Any => Either[Throwable, Any], retType: Type) extends Expr

object Compiled:
  val idCallback: Any => Either[Throwable, Any] = (s: Any) => Right(s)
