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
final case class DateVal(value: LocalDate, evalType: Type, promoteToType: Option[Type]) extends ConstVal:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): DateVal = copy(promoteToType = t)

object DateVal:
  def apply(value: LocalDate): DateVal =
    new DateVal(value = value, evalType = Type.Undefined, promoteToType = None)

  def apply(value: LocalDate, evalType: Type): DateVal =
    new DateVal(value = value, evalType = evalType, promoteToType = None)
