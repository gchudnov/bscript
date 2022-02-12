package com.github.gchudnov.bscript.b1.internal.stdlib.vec

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.symbols.VectorType
import java.time.OffsetDateTime
import com.github.gchudnov.bscript.interpreter.memory.VecCell
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.interpreter.memory.StrCell

final class AppendSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Append" when {
    "there is an empty collection" should {
      "add an element" in {
        val t = Block(
          VarDecl(
            VectorType(TypeRef(typeNames.strType)),
            "x",
            Call(SymbolRef("append"), List(StrVal("Alice"), Vec()))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe VecCell(List(StrCell("Alice")))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "there is a non-empty collection" should {
      "add an element" in {
        val t = Block(
          VarDecl(
            VectorType(TypeRef(typeNames.i32Type)),
            "x",
            Call(SymbolRef("append"), List(IntVal(4), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe VecCell(List(IntCell(1), IntCell(2), IntCell(3), IntCell(4)))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
