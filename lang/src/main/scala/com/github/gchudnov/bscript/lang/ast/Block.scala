package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol }

/**
 * Block of code
 *
 * Contains zero or more expressions
 *
 * If there the block is empty, it evaluates to Void.
 *
 * {{{
 *   {
 *     int x;
 *     { ... } // <- nested block
 *
 *     long y = { "alice"; 1 + 2; }; // here last value is returned and assigned to `y`
 *   }
 * }}}
 *
 * @param exprs
 *   Expressions included in the block
 */
final case class Block(exprs: List[Expr]) extends Expr

object Block:

  lazy val empty: Block =
    new Block(exprs = List.empty[Expr])

  def of(exprs: Expr*): Block =
    Block(exprs.toList)

  extension (block: Block)
    def ++(other: Block): Block =
      Block(exprs = block.exprs ++ other.exprs)

    def :+(other: AST): Block =
      other match
        case x: Block =>
          block ++ x
        case x: Expr =>
          Block(exprs = block.exprs :+ x)
        case _ => sys.error("Cannot append non-Expr to Block")
