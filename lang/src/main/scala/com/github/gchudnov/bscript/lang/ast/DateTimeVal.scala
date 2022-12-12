package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.Type

import java.time.OffsetDateTime

/**
 * DateTime Literal
 *
 * {{{
 *   "2007-12-03T10:15:30+01:00"
 * }}}
 */
final case class DateTimeVal(value: OffsetDateTime) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)
