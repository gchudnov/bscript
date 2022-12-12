package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

import java.time.LocalDate

/**
 * Date Literal
 *
 * {{{
 *   2021-10-12
 * }}}
 */
final case class DateVal(value: LocalDate) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
