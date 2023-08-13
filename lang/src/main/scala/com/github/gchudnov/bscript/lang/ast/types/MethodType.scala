package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*

/**
 * Method Type
 *
 * @param tparams
 *   type parameters
 * @param params
 *   parameters
 * @param retType
 *   return type
 */
final case class MethodType(
  tparams: List[TypeDecl],
  params: List[VarDecl],
  retType: TypeAST,
) extends RealType:
  /**
   * Method signature
   */
  def signature: String =
    val m = tparams.zipWithIndex.map { case (t, i) => t.name -> ('A' + i).toChar.toString() }.toMap

    val tparamsStr = tparams.map(t => m(t.name)).mkString(", ")
    val paramsStr  = params.map(p => m.getOrElse(TypeAST.showTypeAST.show(p.aType), TypeAST.showTypeAST.show(p.aType))).mkString(", ")
    s"<$tparamsStr>($paramsStr)"
