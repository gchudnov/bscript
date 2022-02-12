package com.github.gchudnov.bscript.b1.internal.stdlib.vec

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.BoolCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import java.time.OffsetDateTime

final class ContainsSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "Contains" when {
    "look for an element in the collection" should {
      "return TRUE if the value is in the collection" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(1), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(true)
          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }

      "return FALSE if the value is in the collection" in {
        val t = Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(4), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = B1.run(t)
        errOrRes match
          case Right(cell) =>
            cell mustBe BoolCell(false)
          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }      
    }
  }
