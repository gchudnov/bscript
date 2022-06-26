package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.{ B1, TestSpec }
import com.github.gchudnov.bscript.interpreter.memory.DateCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }

import java.time.LocalDate

final class AdjustDateSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "AdjustDate" when {
    "date is adjusted by a number of days" should {

      /**
       * {{{
       *   {
       *     date x = offsetDate(date("2020-01-01"), 1, "days");
       *     x
       *   }
       * }}}
       */
      "return a new date" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.dateType),
            "x",
            Call(SymbolRef("offsetDate"), List(DateVal(LocalDate.parse("2020-01-01")), IntVal(1), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe DateCell(LocalDate.parse("2020-01-02"))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
