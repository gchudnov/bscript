package com.github.gchudnov.bscript.lang.ast

abstract class AST

abstract class Stat extends AST

abstract class Expr extends Stat

object AST:
  extension (a: AST)
    def +:(block: Block): Block =
      a match
        case x: Block =>
          x ++ block
        case x: Expr =>
          Block(statements = x +: block.statements)
        case _ => sys.error("Cannot prepend non-Expr to Block")
