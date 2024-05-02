package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.asm.stdlib.*
import com.github.gchudnov.bscript.translator.into.asm.stdlib.date.*

private[asm] object AsmPrelude:

  def make(typeNames: TypeNames): Module =
    val isDefineStr = IsDefinedStr(typeNames, typeNames.strType)
    val isDefineI32 = IsDefinedI32(typeNames, typeNames.i32Type)
    val isDefineI64 = IsDefinedI64(typeNames, typeNames.i64Type)
    val isDefineF32 = IsDefinedF32(typeNames, typeNames.f32Type)
    val isDefineF64 = IsDefinedF64(typeNames, typeNames.f64Type)
    val isDefineDate = IsDefinedDat(typeNames, typeNames.dateType)
    val isDefineDateTime = IsDefinedDtm(typeNames, typeNames.datetimeType)

    val methodDecls = List(
      // IsDefined
      isDefineStr.decl,
      isDefineI32.decl,
      isDefineI64.decl,
      isDefineF32.decl,
      isDefineF64.decl,
      isDefineDate.decl,
      isDefineDateTime.decl,
      Now.decl(typeNames),
      //    AdjustDateTime.decl,
      //    AdjustDate.decl,
      //    BetweenTemp.decl,
      //    FieldOfDateTime.decl,
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
      //    Coalesce.decl
    )

    Module(methodDecls*)
