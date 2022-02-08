package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.interpreter.memory.IntCell
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.DeclType
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.types.Types

final class InterpreterSpec extends TestSpec:

  "Interpreter" when {
    "a program is interpreted" should {
      "produce the result" in {

        val typeNames     = IGlobals.typeNames
        val types         = Types.make(typeNames)
        val typeCheckLaws = ITypeCheckLaws.make(types)

        val ast0 = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = for
          astMeta <- Builder.build(ast0, types, typeCheckLaws)
          laws     = IInterpretLaws.make(types, astMeta.meta)
          cell    <- Interpreter.interpret(astMeta.ast, laws, astMeta.meta)
        yield cell

        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(20)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
