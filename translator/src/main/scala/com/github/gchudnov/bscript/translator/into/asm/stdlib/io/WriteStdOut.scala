package com.github.gchudnov.bscript.translator.into.asm.stdlib.io

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split


private[into] object WriteStdOut:

  private val fnName = "writeStdOut"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.voidType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "data"),
      ),
      Block(
        CompiledExpr(callback = WriteStdOut.writeStdOut, retType = TypeRef(typeNames.voidType))
      ),
      Seq(ComAnn("Write a string to stdout"), StdAnn())
    )

  /**
   * Reads the StdIn and returns a string
   */
  private def writeStdOut(s: Any): Either[Throwable, Any] =
    val argSize  = "size"  // size of the buffer

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""console.log(data);
               |""".stripMargin
          )
        )
        yield s.copy(
          lines = lines,
          imports = s.imports,
          inits = s.inits
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
