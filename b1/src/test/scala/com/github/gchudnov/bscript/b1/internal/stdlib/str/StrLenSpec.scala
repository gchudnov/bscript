package com.github.gchudnov.bscript.b1.internal.stdlib.str

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class StrLenSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "StrLen" when {
    "invoked on a string" should {
      "return its length if the string is non-empty" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("strLen"), List(StrVal("Hello World!")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(12)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "return its length if the string is empty" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.i32Type),
            "x",
            Call(SymbolRef("strLen"), List(StrVal("")))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(0)
          case Left(t) =>
            fail("Should be 'right", t)        
      }
    }
  }
