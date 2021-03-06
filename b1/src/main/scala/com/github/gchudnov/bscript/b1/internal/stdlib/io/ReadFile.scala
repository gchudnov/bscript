package com.github.gchudnov.bscript.b1.internal.stdlib.io

import com.github.gchudnov.bscript.b1.B1Exception
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JState

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.control.Exception.allCatch

private[internal] object ReadFile:

  private val fnName = "readFile"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.strType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "path")
      ),
      Block(
        CompiledExpr(callback = ReadFile.readFile, retType = TypeRef(typeNames.strType))
      ),
      Seq(ComAnn("loads text from a file using the provided path"), StdAnn())
    )

  /**
   * Reads file and returns contents as a string.
   *
   * {{{
   *   auto s = readFile("/path/to/file");
   * }}}
   */
  private def readFile(s: Any): Either[Throwable, Any] =
    val argPath = "path" // string

    s match
      case s @ InterpretState(_, _, ms, c) =>
        for
          pathCell <- ms.tryFetch(CellPath(argPath))
          retVal <- pathCell match
                      case StrCell(path) =>
                        for
                          filePath <- allCatch.either(Paths.get(path))
                          contents <- allCatch.either(Files.readString(filePath, StandardCharsets.UTF_8))
                        yield StrCell(contents)
                      case other =>
                        Left(new B1Exception(s"Unexpected type of argument passed to ${fnName}: ${other}"))
        yield s.copy(memSpace = ms, retValue = retVal)

      case s: Scala3State =>
        for
          lines <- Right(
                     split(
                       s"""val errOrContents = for {
                          |  filePath <- allCatch.either(Paths.get(path))
                          |  contents <- allCatch.either(Files.readString(filePath, StandardCharsets.UTF_8))
                          |} yield contents
                          |
                          |errOrContents.toTry.get
                          |""".stripMargin
                     )
                   )
          imports <-
            Right(Seq("scala.util.control.Exception.allCatch", "java.nio.file.Paths", "java.nio.file.Files", "java.nio.charset.StandardCharsets"))
        yield s.copy(lines = lines, imports = s.imports ++ imports)

      case s: Scala3JState =>
        for
          lines <- Right(
                     split(
                       s"""val errOrContents = for {
                          |  filePath <- allCatch.either(Paths.get(path))
                          |  contents <- allCatch.either(Files.readString(filePath, StandardCharsets.UTF_8))
                          |} yield contents
                          |
                          |errOrContents.toTry.get
                          |""".stripMargin
                     )
                   )
          imports <-
            Right(Seq("scala.util.control.Exception.allCatch", "java.nio.file.Paths", "java.nio.file.Files", "java.nio.charset.StandardCharsets"))
        yield s.copy(lines = lines, imports = s.imports ++ imports)

      case other =>
        Left(new B1Exception(s"Unexpected state passed to ${fnName}: ${other}"))
