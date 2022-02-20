package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor

abstract class AST:
  def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R]

object AST:
  extension (a: AST)
    def +:(block: Block): Block =
      a match
        case x: Block =>
          x ++ block
        case x: Expr =>
          Block(statements = x +: block.statements, symbol = block.symbol, evalType = block.evalType, promoteToType = block.promoteToType)
        case _ => sys.error("Cannot prepend non-Expr to Block")
