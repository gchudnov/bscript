package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.{IntCell, NothingCell}
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class CoalesceSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Coalesce" when {
    "two values are provided" should {
      "return the first one if both are defined (defined, defined)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("coalesce"), List(IntVal(1), IntVal(2)))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(1)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "return the first one if the second is undefined (defined, undefined)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("coalesce"), List(IntVal(1), NothingVal()))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(1)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "return the second one if the first one is undefined (undefined, defined)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("coalesce"), List(NothingVal(), IntVal(2)))
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

      "return first undefined if both are undefined (undefined, undefined)" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("coalesce"), List(NothingVal(), NothingVal()))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe NothingCell
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
