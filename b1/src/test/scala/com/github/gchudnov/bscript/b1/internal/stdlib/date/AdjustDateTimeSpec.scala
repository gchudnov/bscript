package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.DateTimeCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class AdjustDateTimeSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "AdjustDateTime" when {
    "datetime is adjusted by a number of days" should {

      /**
       * {{{
       *   {
       *     datetime x = offsetDateTime(datetime("2020-01-01T00:00:00.000Z"), 1, "days");
       *     x
       *   }
       * }}}
       */
      "return a new datetime" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.datetimeType),
            "x",
            Call(SymbolRef("offsetDateTime"), List(DateTimeVal(OffsetDateTime.parse("2020-01-01T00:00:00.000Z")), IntVal(1), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe DateTimeCell(OffsetDateTime.parse("2020-01-02T00:00Z"))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
