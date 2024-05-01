package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.asm.stdlib.*

private[asm] object AsmPrelude:

  def make(typeNames: TypeNames): Module =
    val isDefineStr = IsDefinedT(typeNames, typeNames.strType)
    val isDefineI32 = IsDefinedT(typeNames, typeNames.i32Type)
    val isDefineI64 = IsDefinedT(typeNames, typeNames.i64Type)
    val isDefineF32 = IsDefinedT(typeNames, typeNames.f32Type)
    val isDefineF64 = IsDefinedT(typeNames, typeNames.f64Type)
    val isDefineDate = IsDefinedT(typeNames, typeNames.dateType)
    val isDefineDateTime = IsDefinedT(typeNames, typeNames.datetimeType)

    val methodDecls = List(
      isDefineStr.decl,
      isDefineI32.decl,
      isDefineI64.decl,
      isDefineF32.decl,
      isDefineF64.decl,
      isDefineDate.decl,
      isDefineDateTime.decl,
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

    Module(methodDecls*)
