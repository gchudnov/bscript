package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.into.asm.stdlib.*
import com.github.gchudnov.bscript.translator.into.asm.stdlib.date.*
import com.github.gchudnov.bscript.translator.into.asm.stdlib.num.*

private[asm] object AsmPrelude:

  def make(typeNames: TypeNames): Module =
    val isDefinedStr = IsDefinedStr(typeNames, typeNames.strType)
    val isDefinedI32 = IsDefinedI32(typeNames, typeNames.i32Type)
    val isDefinedI64 = IsDefinedI64(typeNames, typeNames.i64Type)
    val isDefinedF32 = IsDefinedF32(typeNames, typeNames.f32Type)
    val isDefinedF64 = IsDefinedF64(typeNames, typeNames.f64Type)
    val isDefinedDate = IsDefinedDat(typeNames, typeNames.dateType)
    val isDefinedDateTime = IsDefinedDtm(typeNames, typeNames.datetimeType)

    val coalesceStr = CoalesceStr(typeNames, typeNames.strType)
    val coalesceI32 = CoalesceI32(typeNames, typeNames.i32Type)
    val coalesceI64 = CoalesceI64(typeNames, typeNames.i64Type)
    val coalesceF32 = CoalesceF32(typeNames, typeNames.f32Type)
    val coalesceF64 = CoalesceF64(typeNames, typeNames.f64Type)
    val coalesceDate = CoalesceDat(typeNames, typeNames.dateType)
    val coalesceDateTime = CoalesceDtm(typeNames, typeNames.datetimeType)
    
    val methodDecls = List(
      isDefinedStr.decl,
      isDefinedI32.decl,
      isDefinedI64.decl,
      isDefinedF32.decl,
      isDefinedF64.decl,
      isDefinedDate.decl,
      isDefinedDateTime.decl,
      Now.decl(typeNames),
      Today.decl(typeNames),
      RoundF64.decl(typeNames),
      RoundF32.decl(typeNames),
      TruncateF64.decl(typeNames),
      TruncateF32.decl(typeNames),
      coalesceStr.decl,
      coalesceI32.decl,
      coalesceI64.decl,
      coalesceF32.decl,
      coalesceF64.decl,
      coalesceDate.decl,
      coalesceDateTime.decl,
      //    AdjustDateTime.decl,
      //    AdjustDate.decl,
      //    BetweenTemp.decl,
      //    FieldOfDateTime.decl,
      //    SetDateTime.decl,
      //    Printf.decl,
      //    SPrintf.decl,
      //    ReadFile.decl,
      //    ExactInt.decl,
      //    ExactLong.decl,
      //    StrLen.decl,
      //    Append.decl,
      //    Contains.decl,
    )

    Module(methodDecls*)
