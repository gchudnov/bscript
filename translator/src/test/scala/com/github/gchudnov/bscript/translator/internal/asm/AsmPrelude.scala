package com.github.gchudnov.bscript.translator.internal.asm

import com.github.gchudnov.bscript.translator.internal.asm.stdlib.*
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.date.*
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.io.*
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.num.*
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.str.*
import com.github.gchudnov.bscript.translator.internal.asm.stdlib.vec.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[asm] object AsmPrelude:

  private val methods = List(
    AdjustDateTime.decl,
    AdjustDate.decl,
    BetweenTemp.decl,
    FieldOfDateTime.decl,
    Now.decl,
    SetDateTime.decl,
    Today.decl,
    Printf.decl,
    SPrintf.decl,
    ReadFile.decl,
    ExactInt.decl,
    ExactLong.decl,
    Round.decl,
    Truncate.decl,
    StrLen.decl,
    Append.decl,
    Contains.decl,
    IsDefined.decl,
    Coalesce.decl
  )

  def make(typeNames: TypeNames): Block =
    val methodDecls = methods.map(_(typeNames))
    Block(methodDecls*)
