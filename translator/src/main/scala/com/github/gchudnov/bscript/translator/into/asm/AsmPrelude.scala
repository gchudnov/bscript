package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.asm.stdlib.*

private[asm] object AsmPrelude:

  private val methods = List(
    IsDefinedStr.decl,
//    AdjustDateTime.decl,
//    AdjustDate.decl,
//    BetweenTemp.decl,
//    FieldOfDateTime.decl,
//    Now.decl,
//    SetDateTime.decl,
//    Today.decl,
//    Printf.decl,
//    SPrintf.decl,
//    ReadFile.decl,
//    ExactInt.decl,
//    ExactLong.decl,
//    Round.decl,
//    Truncate.decl,
//    StrLen.decl,
//    Append.decl,
//    Contains.decl,
//    IsDefined.decl,
//    Coalesce.decl
  )

  def make(typeNames: TypeNames): Module =
    val methodDecls = methods.map(_(typeNames))
    Module(methodDecls*)
