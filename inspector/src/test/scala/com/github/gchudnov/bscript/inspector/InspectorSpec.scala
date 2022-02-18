package com.github.gchudnov.bscript.inspector

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import com.github.gchudnov.bscript.inspector.internal.InspectorTypeNames

final class InspectorSpec extends TestSpec:
  val typeNames = InspectorTypeNames.make()

  "Inspector" when {
    "AST is inspected" should {

      /**
       * {{{
       *   // globals
       *   int y = 1;
       *
       *   void main() { y = 2; }  // here after execution, we should wrap this method call
       *
       *   main();
       * }}}
       */
      "wrap AST to trace memory between function calls" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "main",
            List.empty[ArgDecl],
            Block(
              Assign(
                Var(SymbolRef("y")),
                IntVal(2)
              )
            )
          ),
          Call(SymbolRef("main"), List.empty[Expr])
        )

        val errOrRes = Inspector.memWatch(CellPath("y"), t, typeNames)
        errOrRes match
          case Right(actual) =>
            val block      = actual.asInstanceOf[Block]
            val methodDecl = block.statements(0).asInstanceOf[MethodDecl]
            val callBlock  = block.statements(3).asInstanceOf[Block] // NOTE: `Call(SymbolRef("main"), List.empty[Expr])` is wrapped in a `Block`

            methodDecl.name mustBe ("memWatch")

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
