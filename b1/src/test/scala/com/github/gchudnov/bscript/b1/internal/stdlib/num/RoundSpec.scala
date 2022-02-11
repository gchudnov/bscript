package com.github.gchudnov.bscript.b1.internal.stdlib.num

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.FloatCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class RoundSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Round" when {
    "a number is rounded" should {
      "round f32" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.f32Type),
            "x",
            Call(SymbolRef("round"), List(FloatVal(3.1234f), IntVal(2)))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe FloatCell(3.12f)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
