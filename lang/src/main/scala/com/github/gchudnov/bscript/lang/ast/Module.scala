package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ Symbol, Type }
import scala.collection.immutable.Seq

/**
 * A collection of statements
 * 
 * {{{
 *   let a = 10;
 *   a + 10;
 * }}}
 *
 * @param statements
 *   Statements included in the module
 */
final case class Module(statements: List[Expr], symbol: Symbol, evalType: Type, promoteToType: Option[Type]) extends Expr:
  override def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R] =
    v.visit(s, this)

  override def withPromoteToType(t: Option[Type]): Module = copy(promoteToType = t)

object Module:

  val empty: Module =
    new Module(statements = List.empty[Expr], symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  def apply(statements: Expr*): Module =
    ofSeq(statements.toList)

  def apply(statements: Seq[Expr], symbol: Symbol, evalType: Type): Module =
    new Module(statements = statements.toList, symbol = symbol, evalType = evalType, promoteToType = None)

  def ofSeq(statements: Seq[Expr]): Module =
    new Module(statements = statements.toList, symbol = Symbol.Undefined, evalType = Type.Undefined, promoteToType = None)

  extension (block: Module)
    def ++(other: Module): Module =
      if block.evalType == other.evalType && block.promoteToType == other.promoteToType && block.symbol == other.symbol then
        Module(statements = block.statements ++ other.statements, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
      else sys.error("Cannot join Modules with different evalType and promoteToType values")

    def :+(other: AST): Module =
      other match
        case x: Module =>
          block ++ x
        case x: Expr =>
          Module(statements = block.statements :+ x, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
        case _ => sys.error("Cannot append non-Expr to Module")
