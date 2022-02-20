package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
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
final case class Block(statements: List[Expr], symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Block = copy(promoteToType = t)

object Block:

  val empty: Block =
    new Block(statements = List.empty[Expr], symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(statements: Expr*): Block =
    new Block(statements = statements.toList, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(statements: Seq[Expr], symbol: Symbol, evalType: Type): Block =
    new Block(statements = statements.toList, symbol = symbol, evalType = evalType, promoteToType = None)

  extension (block: Block)
    def ++(other: Block): Block =
      if block.evalType == other.evalType && block.promoteToType == other.promoteToType && block.symbol == other.symbol then
        Block(statements = block.statements ++ other.statements, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
      else sys.error("Cannot join Blocks with different evalType and promoteToType values")

    def :+(other: AST): Block =
      other match
        case x: Block =>
          block ++ x
        case x: Expr =>
          Block(statements = block.statements :+ x, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
        case _ => sys.error("Cannot append non-Expr to Block")
