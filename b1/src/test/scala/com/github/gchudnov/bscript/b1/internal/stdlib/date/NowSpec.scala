package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.DateTimeCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime
import java.time.ZoneId

final class NowSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Now" when {
    "invoked" should {
      "return current date and time" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.datetimeType),
            "x",
            Call(SymbolRef("now"), List.empty[Expr])
          ),
          Var(SymbolRef("x"))
        )

        val expectedAtLeast = OffsetDateTime.now(ZoneId.of("Z"))

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            val dateTimeCell = cell.asInstanceOf[DateTimeCell]
            dateTimeCell.value.compareTo(expectedAtLeast) mustBe 1
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
