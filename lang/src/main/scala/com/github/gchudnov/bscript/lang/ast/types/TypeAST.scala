package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * A Type in the AST
 */
abstract class TypeAST extends AST

object TypeAST:
  given showTypeAST: Show[TypeAST] = new Show[TypeAST]:
    override def show(a: TypeAST): String =
      a match
        case Auto()                      => "auto"
        case TypeId(name)                => name
        case VecType(elemType)           => s"vec<${showTypeAST.show(elemType)}>"
        case SetType(keyType)            => s"set<${showTypeAST.show(keyType)}>"
        case MapType(keyType, valueType) => s"map<${showTypeAST.show(keyType)},${showTypeAST.show(valueType)}>"
        case StructType(tfields, fields) =>
          val tfieldsStr = tfields.zipWithIndex.map { case (t, i) => ('A' + i).toChar.toString }.mkString(",")
          val fieldsStr  = fields.map(f => showTypeAST.show(f.aType)).mkString(",")
          s"struct<$tfieldsStr>{$fieldsStr}"
        case MethodType(tparams, params, retType) =>
          val tparamsStr = tparams.map(t => t.name).mkString(",")
          val paramsStr  = params.map(p => showTypeAST.show(p.aType)).mkString(",")
          s"method<$tparamsStr>($paramsStr): ${showTypeAST.show(retType)}"
        case GenericType(name) => name
        case BuiltInType(name) => name
        case ByName(expr)      => s"=>${showTypeAST.show(expr)}"

  /**
   * Checks if the type is nothing
   */
  def isNothing(t: TypeAST): Boolean =
    isBuiltInType(t, TypeName.nothing)

  /**
   * Checks if the type is void
   */
  def isVoid(t: TypeAST): Boolean =
    isBuiltInType(t, TypeName.void)

  /**
   * Checks if the type is built-in
   */
  private def isBuiltInType(t: TypeAST, name: String): Boolean =
    t match
      case BuiltInType(tname) if tname == name => true
      case _                                   => false
