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
  override def asString: String =
    val tparamsStr = tparams.map(_.name).mkString(", ")
    val paramsStr  = params.map(_.aType.asString).mkString(", ")
    s"method<$tparamsStr>($paramsStr): ${retType.asString}"

  /**
   * Method signature
   */
  def signature: String =
    val m = tparams.zipWithIndex.map { case (t, i) => t.name -> ('A' + i).toChar.toString() }.toMap

    val tparamsStr = tparams.map(t => m(t.name)).mkString(", ")
    val paramsStr  = params.map(p => m.getOrElse(p.aType.asString, p.aType.asString)).mkString(", ")
    s"<$tparamsStr>($paramsStr)"
