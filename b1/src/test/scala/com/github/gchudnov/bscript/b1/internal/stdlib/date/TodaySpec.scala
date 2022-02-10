package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.DateCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime
import java.time.LocalDate
import java.time.ZoneId

final class TodaySpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Today" when {
    "invoked" should {
      "returns the current date" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.dateType),
            "x",
            Call(SymbolRef("today"), List.empty[Expr])
          ),
          Var(SymbolRef("x"))
        )

        val expectedAtLeast = LocalDate.now(ZoneId.of("Z"))

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            val dateCell = cell.asInstanceOf[DateCell]
            dateCell.value.compareTo(expectedAtLeast) mustBe 0 // NOTE: there is a small chance that during the test the day changes
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
