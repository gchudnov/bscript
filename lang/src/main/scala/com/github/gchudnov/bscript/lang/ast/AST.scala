package com.github.gchudnov.bscript.lang.ast

/**
 * {{{
 * AST -+
 *      |
 *      +- Stat -+ // TODO: remove statements
 *      |        |
 *      |        +- Expr +- Ref +- Access
 *      |                |      +- Id
 *      |                |
 *      |                +- Decl +- MethodDecl
 *      |                |       +- StructDecl
 *      |                |       +- VarDecl
 *      |                |       +- TypeDecl
 *      |                |       +- BuiltInDecl
 *      |                |
 *      |                +- Annotated
 *      |                +- Assign
 *      |                +- Block
 *      |                +- Call
 *      |                +- Compiled
 *      |                +- If
 *      |                +- Init
 *      |                +- Return
 *      |                +- Pair
 *      |                |
 *      |                +- Lit +- ConstLit
 *      |                       +- CollectionLit
 *      |                       +- MethodLit
 *      |
 *      +- TypeAST +- Auto
 *                 +- TypeId
 *                 +- ByName
 *                 +- RealType +- VecType
 *                             +- SetType
 *                             +- MapType
 *                             +- StructType
 *                             +- MethodType
 *                             +- GenericType
 *                             +- BuiltInType
 *
 * Const -+- BoolVal   TODO: add AnyVal ? // instead of many number types, use just one - number
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
    def +:(b: Block): Block =
      a match
        case x: Block =>
          x ++ b
        case x: Expr =>
          Block(exprs = x +: b.exprs)
        case _ => sys.error("Cannot prepend non-Expr to Block")
