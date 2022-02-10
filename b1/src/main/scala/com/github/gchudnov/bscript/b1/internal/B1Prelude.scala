package com.github.gchudnov.bscript.b1.internal

import com.github.gchudnov.bscript.b1.internal.stdlib.*
import com.github.gchudnov.bscript.b1.internal.stdlib.io.*
import com.github.gchudnov.bscript.b1.internal.stdlib.num.*
import com.github.gchudnov.bscript.b1.internal.stdlib.str.*
import com.github.gchudnov.bscript.b1.internal.stdlib.date.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[b1] object B1Prelude:

  private val methods = List(
    AdjustDateTime.decl,
    FieldOfDateTime.decl,
    Now.decl,
    SetDateTime.decl,
    Today.decl,
    Printf.decl,
    ReadFile.decl,
    Round.decl,
    Truncate.decl,
    StrLen.decl,
    IsDefined.decl,
    Coalesce.decl
  )

  def make(typeNames: TypeNames): Block =
    val methodDecls = methods.map(_(typeNames))
    Block(methodDecls*)