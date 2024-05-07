package com.github.gchudnov.bscript.translator.into.asm.stdlib.io

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split


private[into] object ReadStdIn:

  private val fnName = "readStdIn"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.strType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.i32Type), "size"),
      ),
      Block(
        CompiledExpr(callback = ReadStdIn.readStdIn, retType = TypeRef(typeNames.strType))
      ),
      Seq(ComAnn("Read a string from stdin"), StdAnn())
    )

  /**
   * Reads the StdIn and returns a string
   */
  private def readStdIn(s: Any): Either[Throwable, Any] =
    val argSize  = "size"  // size of the buffer

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""const buffer = new ArrayBuffer(${argSize});
               |process.stdin.read(buffer);
               |return String.UTF8.decode(buffer);
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
