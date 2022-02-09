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
    Printf.decl,
    StrLen.decl,
    AdjustDateTime.decl,
    FieldOfDateTime.decl,
    SetDateTime.decl,
    IsDefined.decl,
    Coalesce.decl,
    Today.decl,
    Now.decl,
    Round.decl,
    Truncate.decl
  )

  def make(typeNames: TypeNames): Block =
    val methodDecls = methods.map(_(typeNames))
    Block(methodDecls*)
