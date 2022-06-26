package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.{ B1, TestSpec }
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef }

final class CastIntSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "CastInt" when {
    "double to int" should {

      /**
       * {{{
       *   {
       *     double x = 12.0;
       *     int y = exactInt(x);
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
            TypeRef(typeNames.i32Type),
            "y",
            Call(SymbolRef("exactInt"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(12)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      //

      /**
       * {{{
       *   {
       *     double x = 12.34;
       *     int y = exactInt(x);
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
            TypeRef(typeNames.i32Type),
            "y",
            Call(SymbolRef("exactInt"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage.contains("Rounding necessary") mustBe true
      }
    }

    "long to int" should {

      /**
       * {{{
       *   {
       *     long x = 123;
       *     int y = exactInt(x);
       *     y
       *   }
       * }}}
       */
      "cast if within range" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i64Type),
            "x",
            LongVal(123)
          ),
          VarDecl(
            TypeRef(typeNames.i32Type),
            "y",
            Call(SymbolRef("exactInt"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(123)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   {
       *     long x = Long.MAX_VALUE;
       *     int y = exactInt(x);
       *     // throws
       *   }
       * }}}
       */
      "not cast if out of range" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i64Type),
            "x",
            LongVal(Long.MaxValue)
          ),
          VarDecl(
            TypeRef(typeNames.i32Type),
            "y",
            Call(SymbolRef("exactInt"), List(Var(SymbolRef("x"))))
          ),
          Var(SymbolRef("y"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            println(t.getMessage)
            t.getMessage.contains("integer overflow") mustBe true
      }
    }
  }
