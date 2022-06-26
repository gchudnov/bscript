package com.github.gchudnov.bscript.b1.internal.stdlib.date

import com.github.gchudnov.bscript.b1.{ B1, TestSpec }
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }

import java.time.LocalDate

final class BetweenDatesSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "BetweenDates" when {
    "two dates (a, b) are subtracted" should {

      /**
       * {{{
       *   {
       *     val dd = betweenDates(date("2022-02-01"), date("2022-03-01"), "days");
       *     dd
       *   }
       * }}}
       */
      "provide the output if b >= a" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "dd",
            Call(SymbolRef("betweenDates"), List(DateVal(LocalDate.parse("2022-02-01")), DateVal(LocalDate.parse("2022-03-01")), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(100)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   {
       *     val dd = betweenDates(date("2022-03-01"), date("2022-02-01"), "days");
       *     dd
       *   }
       * }}}
       */
      "report an error if b < a" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "dd",
            Call(SymbolRef("betweenDates"), List(DateVal(LocalDate.parse("2022-03-01")), DateVal(LocalDate.parse("2022-02-01")), StrVal("days")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            println(t.getMessage)
            t.getMessage.contains("XXX") mustBe true
      }
    }
  }
