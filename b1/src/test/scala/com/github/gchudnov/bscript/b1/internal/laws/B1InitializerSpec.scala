package com.github.gchudnov.bscript.b1.internal.laws

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.TestSpec
import com.github.gchudnov.bscript.interpreter.memory.{ IntCell, StructCell }
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef

final class B1InitializerSpec extends TestSpec:
  private val typeNames = B1.typeNames

  "B1Initializer" when {
    "struct is initialize" should {
//      // NOTE: this is not working at the moment
//      "initialize fields in the order they are defined" in {
//        val t = Block(
//          StructDecl(
//            "A",
//            List(
//              FieldDecl(TypeRef(typeNames.i32Type), "a"),
//              FieldDecl(TypeRef(typeNames.i32Type), "b"),
//              FieldDecl(TypeRef(typeNames.i32Type), "c"),
//            )
//          ),
//          VarDecl(
//            TypeRef("A"),
//            "x",
//            StructVal(
//              TypeRef("A"),
//              Map(
//                "a" -> IntVal(1),
//                "b" -> Add(IntVal(1), Var(SymbolRef("a"))),
//                "c" -> Add(IntVal(1), Var(SymbolRef("b"))),
//              )
//            )
//          )
//        )
//
//        val errOrRes = B1.run(t)
//        errOrRes match
//          case Right(cell) =>
//            cell mustBe StructCell(
//              Map(
//                "a" -> IntCell(1),
//                "b" -> IntCell(2),
//                "c" -> IntCell(3),
//              )
//            )
//          case Left(t) =>
//            fail("Should be 'right", t)
//      }
    }
  }
