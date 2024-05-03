package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import scala.collection.immutable.Seq

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
    ofSeq(statements.toList)

  def apply(statements: Seq[Expr], symbol: Symbol, evalType: Type): Block =
    new Block(statements = statements.toList, symbol = symbol, evalType = evalType, promoteToType = None)

  def ofSeq(statements: Seq[Expr]): Block =
    new Block(statements = statements.toList, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  extension (block: Block)
    def ++(other: Block): Block =
      Block(statements = block.statements ++ other.statements, symbol = other.symbol, evalType = other.evalType, promoteToType = other.promoteToType)

    def :+(other: AST): Block =
      other match
        case x: Block =>
          block ++ x
        case x: Expr =>
          Block(statements = block.statements :+ x, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
        case _ => sys.error("Cannot append non-Expr to Block")
