package com.github.gchudnov.bscript.translator.internal.asm.stdlib.io

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames

private[internal] object Printf:

  private val fnName = "printf"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.voidType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "format"),
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block.empty, // NOTE: at the moment it is not implemented
      Seq(ComAnn("Prints the formatted value to StdOut"), StdAnn())
    )
