package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.{ B1, TestSpec }
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }

final class CastLongSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "CastLong" when {
    "double to long" should {

      /**
       * {{{
       *   {
       *     double x = 12.0;
       *     long y = exactLong(x);
       *     y
       *   }
       * }}}
       */
      "cast if exact" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.f64Type),
            "x",
            DoubleVal(12.0)
          ),
          VarDecl(
            TypeRef(typeNames.i64Type),
            "y",
            Call(SymbolRef("exactLong"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe LongCell(12)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      //

      /**
       * {{{
       *   {
       *     double x = 12.34;
       *     long y = exactLong(x);
       *     // throws
       *   }
       * }}}
       */
      "not cast if not exact" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.f64Type),
            "x",
            DoubleVal(12.34)
          ),
          VarDecl(
            TypeRef(typeNames.i64Type),
            "y",
            Call(SymbolRef("exactLong"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage.contains("Cannot convert double") mustBe true
      }
    }

    "dec to long" should {

      /**
       * {{{
       *   {
       *     dec x = 123;
       *     long y = exactLong(x);
       *     y
       *   }
       * }}}
       */
      "cast if within range" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.decType),
            "x",
            DecimalVal(BigDecimal("123"))
          ),
          VarDecl(
            TypeRef(typeNames.i64Type),
            "y",
            Call(SymbolRef("exactLong"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe LongCell(123)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   {
       *     dec x = 12345678901234567890;
       *     long y = exactLong(x);
       *     // throws
       *   }
       * }}}
       */
      "not cast if out of range" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.decType),
            "x",
            DecimalVal(BigDecimal("12345678901234567890"))
          ),
          VarDecl(
            TypeRef(typeNames.i64Type),
            "y",
            Call(SymbolRef("exactLong"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage.contains("Cannot convert bigDecimal") mustBe true
      }
    }
  }
