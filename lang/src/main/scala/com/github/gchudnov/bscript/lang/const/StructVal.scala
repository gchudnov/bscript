package com.github.gchudnov.bscript.lang.const

import com.github.gchudnov.bscript.lang.ast.Expr
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Struct Initializer
 *
 * {{{
 *   struct A {
 *     a: int
 *     b: string
 *   };
 *
 *   A = { a: 1, b: "hello" };
 * }}}
 */
final case class StructVal(sType: Type, value: Map[String, Expr]) extends ConstVal

object StructVal:

  def apply(sType: Type): StructVal =
    new StructVal(sType = sType, value = Map.empty[String, Expr])
