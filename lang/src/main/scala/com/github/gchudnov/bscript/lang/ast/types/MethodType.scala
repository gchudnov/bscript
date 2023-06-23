package com.github.gchudnov.bscript.lang.ast.types

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*

/**
  * Method Type
  *
  * @param tparams type parameters
  * @param params parameters
  * @param retType return type
  */
final case class MethodType(
  tparams: List[TypeDecl],
  params: List[VarDecl],
  retType: TypeAST
) extends TypeAST:
  override def asString: String =
    val tparamsStr = tparams.zipWithIndex.map { case (t, i) => ('A' + i).toChar.toString }.mkString(", ")
    val paramsStr  = params.map(_.vType.asString).mkString(", ")
    s"method<$tparamsStr>($paramsStr): ${retType.asString}"

