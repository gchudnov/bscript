package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }

/**
 * Block of code
 *
 * Contains zero or more statements
 * {{{
 *   {
 *     int x;
 *     { ... } // <- nested block
 *
 *     long y = { "alice"; 1 + 2; }; // here last value is returned and assigned to `y`
 *   }
 * }}}
 *
 * @param statements
 *   Statements included in the block
 */
final case class Block(statements: List[Expr]) extends Expr

object Block:

  val empty: Block =
    new Block(statements = List.empty[Expr])

  def of(statements: Expr*): Block =
    Block(statements.toList)

  extension (block: Block)
    def ++(other: Block): Block =
      Block(statements = block.statements ++ other.statements)

    def :+(other: AST): Block =
      other match
        case x: Block =>
          block ++ x
        case x: Expr =>
          Block(statements = block.statements :+ x)
        case _ => sys.error("Cannot append non-Expr to Block")
