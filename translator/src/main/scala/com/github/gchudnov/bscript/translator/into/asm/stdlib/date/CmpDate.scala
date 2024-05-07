package com.github.gchudnov.bscript.translator.into.asm.stdlib.date


import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState}
import com.github.gchudnov.bscript.translator.into.scala3.Scala3State
import com.github.gchudnov.bscript.translator.into.scalax.scala3j.Scala3JState

import java.time.LocalDate
import scala.util.control.Exception.allCatch

/**
 * Date Comparison
 * @param op operator to use: < >, ...
 */
final class CmpDate(typeNames: TypeNames, op: String):
  import DateTime.*

  private val suffixes: Map[String, String] =
    Map(
      "<" -> "lt",
      ">" -> "gt"
    )

  private val fnName = s"cmp_date_${suffixes(op)}"

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.boolType),
      fnName,
      List(
        ArgDecl(TypeRef(typeNames.dateType), "lhs"),
        ArgDecl(TypeRef(typeNames.dateType), "rhs"),
      ),
      Block(
        CompiledExpr(callback = this.cmpDate, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("Offsets the provided date"), StdAnn())
    )

  /**
   * Compare dates
   */
  private def cmpDate(s: Any): Either[Throwable, Any] =
    val argLhs  = "lhs"  // date
    val argRhs  = "rhs"  // date

    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""return (${argLhs}.getTime() ${op} ${argRhs}.getTime());
               |""".stripMargin
          )
        )
        yield s.copy(lines = lines, imports = s.imports)

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
