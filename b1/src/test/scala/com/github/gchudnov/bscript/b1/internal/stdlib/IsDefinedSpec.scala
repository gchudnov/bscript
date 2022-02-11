package com.github.gchudnov.bscript.b1.internal.stdlib

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.BoolCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class IsDefinedSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "IsDefined" when {
    "invoked" should {
      "return TRUE for defined variable" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("isDefined"), List(IntVal(1)))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(true)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "return FALSE for the undefined variable" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("isDefined"), List(NothingVal()))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(false)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
