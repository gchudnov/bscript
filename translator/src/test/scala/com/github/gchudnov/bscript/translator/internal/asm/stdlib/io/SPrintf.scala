package com.github.gchudnov.bscript.translator.internal.asm.stdlib.io

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.translator.internal.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.internal.scalax.scala3j.Scala3JState

import scala.util.control.Exception.allCatch
import java.text.DecimalFormat
import java.time.{LocalDate, OffsetDateTime}
import java.time.format.DateTimeFormatter


private[internal] object SPrintf {
  private val fnName = "sprintf"

  def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.strType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "format"),
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block(
        CompiledExpr(callback = SPrintf.sprintf, retType = TypeRef(typeNames.strType))
      ),
      Seq(ComAnn("Prints the formatted value to a string"), StdAnn())
    )

  /**
   * Prints the formatted value to a string
   *
   * {{{
   *   int x = null;
   *   int y = 17;
   *
   *   string r1 = sprintf("%p", x);
   *   // null
   *
   *   string r2 = sprintf("%d", 7);
   *   // 17
   * }}}
   */
  private def sprintf(s: Any): Either[Throwable, Any] =
    val argFormat = "format" // string
    val argValue = "value" // auto

    def formatDecimal(format: String, value: BigDecimal): Either[Throwable, String] =
      allCatch.either(new DecimalFormat(format).format(value))

    def formatDate(format: String, value: LocalDate): Either[Throwable, String] =
      allCatch.either(value.format(DateTimeFormatter.ofPattern(format)))

    def formatDateTime(format: String, value: OffsetDateTime): Either[Throwable, String] =
      allCatch.either(value.format(DateTimeFormatter.ofPattern(format)))

    s match
      case s: AsmState =>
        Right(s) // TODO: change later

      case s: Scala3State =>
        for lines <- Right(
          split(
            s"""// NOTE: Add [T] to the method
               |${argValue} match {
               |  case value: BigDecimal =>
               |    new DecimalFormat(format).format(value)
               |  case value: LocalDate =>
               |    value.format(DateTimeFormatter.ofPattern(format))
               |  case value: OffsetDateTime =>
               |    value.format(DateTimeFormatter.ofPattern(format))
               |  case _ =>
               |    throw new RuntimeException(s"Unexpected value of time was passed to ${fnName}: '$${${argValue}}'")
               |}
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines, imports = s.imports + "java.text.DecimalFormat")

      case s: Scala3JState =>
        for lines <- Right(
          split(
            s"""// NOTE: Add [T] to the method
               |${argValue} match {
               |  case value: BigDecimal =>
               |    new DecimalFormat(format).format(value)
               |  case value: LocalDate =>
               |    value.format(DateTimeFormatter.ofPattern(format))
               |  case value: OffsetDateTime =>
               |    value.format(DateTimeFormatter.ofPattern(format))
               |  case _ =>
               |    throw new RuntimeException(s"Unexpected value of time was passed to ${fnName}: '$${${argValue}}'")
               |}
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines, imports = s.imports + "java.text.DecimalFormat")

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))


}
