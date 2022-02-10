package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class FieldOfDateTimeSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "FieldOfDateTime" when {
    "a field is extracted from datetime" should {
      "return days" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("fieldOfDateTime"), List(DateTimeVal(OffsetDateTime.parse("2020-01-02T03:04:05.000Z")), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(2)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
