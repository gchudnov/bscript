package com.github.gchudnov.bscript.b1.internal.laws

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.{ BoolCell, DateTimeCell }
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef

import java.time.{ LocalDate, OffsetDateTime }

final class B1ComparatorSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "B1Comparator" when {
    "comparing values" should {

      /**
       * {{{
       *   {
       *     date x = date("2022-04-01");
       *     date y = date("2023-01-01");
       *
       *     x < y
       *   }
       * }}}
       */
      "compare (date, date)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.dateType),
            "x",
            DateVal(LocalDate.parse("2020-01-01"))
          ),
          VarDecl(
            TypeRef(typeNames.dateType),
            "y",
            DateVal(LocalDate.parse("2020-03-01"))
          ),
          LessEqual(Var(SymbolRef("x")), Var(SymbolRef("y")))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(true)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   {
       *     dateTime x = dateTime("2020-01-01T00:00:00.000Z");
       *     dateTime y = dateTime("2020-03-01T00:00:00.000Z");
       *
       *     x <= y
       *   }
       * }}}
       */
      "compare (dateTime, dateTime)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.datetimeType),
            "x",
            DateTimeVal(OffsetDateTime.parse("2020-01-01T00:00:00.000Z"))
          ),
          VarDecl(
            TypeRef(typeNames.datetimeType),
            "y",
            DateTimeVal(OffsetDateTime.parse("2020-03-01T00:00:00.000Z"))
          ),
          LessEqual(Var(SymbolRef("x")), Var(SymbolRef("y")))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(true)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
