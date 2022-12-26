package com.github.gchudnov.bscript.lang.ast

/**
 * {{{
 * AST -+
 *      |
 *      +- Stat -+
 *               |
 *               +- Expr +- Ref +- Access
 *                       |      +- Id
 *                       |
 *                       +- Decl +- MethodDecl
 *                       |       +- StructDecl
 *                       |       +- VarDecl
 *                       |
 *                       +- Assign
 *                       +- Block
 *                       +- Call
 *                       +- Compiled
 *                       +- If
 *                       +- Init
 *                       +- Literal
 *                       +- Vec // ??
 *
 * Const -+- BoolVal
 *        +- ByteVal
 *        +- CharVal
 *        +- DateTimeVal
 *        +- DateVal
 *        +- DecVal
 *        +- DoubleVal
 *        +- FloatVal
 *        +- IntVal
 *        +- LongVal
 *        +- NullVal
 *        +- ShortVal
 *        +- StrVal
 *        +- VoidVal
 * }}}
 */
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
          Block(exprs = x +: block.exprs)
        case _ => sys.error("Cannot prepend non-Expr to Block")
