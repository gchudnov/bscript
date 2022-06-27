package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.{ B1, TestSpec }
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }

import java.time.LocalDate

final class BetweenTempSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "BetweenDates" when {
    "two dates (a, b) are subtracted" should {

      /**
       * {{{
       *   {
       *     val x = betweenTemp(date("2022-02-01"), date("2022-03-01"), "days");
       *     x
       *   }
       * }}}
       */
      "positive value if a < b" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("betweenTemp"), List(DateVal(LocalDate.parse("2022-02-01")), DateVal(LocalDate.parse("2022-03-01")), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(28)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   {
       *     val x = betweenTemp(date("2022-03-01"), date("2022-02-01"), "days");
       *     x
       *   }
       * }}}
       */
      "negative value if a > b" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("betweenTemp"), List(DateVal(LocalDate.parse("2022-03-01")), DateVal(LocalDate.parse("2022-02-01")), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(-28)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
