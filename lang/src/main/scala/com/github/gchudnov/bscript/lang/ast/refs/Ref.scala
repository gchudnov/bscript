package com.github.gchudnov.bscript.lang.ast.refs

import com.github.gchudnov.bscript.lang.ast.Expr

/**
 * Identifier, Id or Access
 *
 * {{{
 * AST -+
 *      |
 *      +- Stat -+
 *      |        |
 *      |        +- Expr +- Ref +- Access
 *      |                |      +- Id
 *      .                .
 * }}}
 */
abstract class Ref extends Expr:
  def path: List[String]
