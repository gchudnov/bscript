package com.github.gchudnov.bscript.lang.ast

/**
 * {{{
 * AST -+
 *      |
 *      +- Stat -+
 *      |        |
 *      |        +- Expr +- Ref +- Access
 *      |                |      +- Id
 *      |                |
 *      |                +- Decl +- MethodDecl // TODO: refactor declarations (?), see cpp2
 *      |                |       +- StructDecl
 *      |                |       +- VarDecl
 *      |                |       +- TypeDecl
 *      |                |
 *      |                +- Annotated
 *      |                +- Assign
 *      |                +- Block
 *      |                +- Call
 *      |                +- Compiled
 *      |                +- If
 *      |                +- Init
 *      |                +- KeyValue
 *      |                |
 *      |                +- Lit +- ConstLit
 *      |                       +- CollectionLit
 *      |                       +- MethodLit
 *      |
 *      +- TypeAST -+- Auto
 *                  +- TypeId
 *                  +- VecType
 *                  +- MapType
 *                  +- StructType
 *                  +- MethodType
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
 *
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
