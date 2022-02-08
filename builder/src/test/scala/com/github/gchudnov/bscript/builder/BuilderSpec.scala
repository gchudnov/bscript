package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ TypeRef, DeclType, SymbolRef }

final class BuilderSpec extends TestSpec:
  "Builder" when {
    "AST is build" should {
      "resolve scopes, define symbols and assign types" in {
        
        val typeNames = BGlobals.typeNames
        val typeCheckLaws = BTypeCheckLaws.make()

        val ast0 = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = Builder.build(ast0, typeNames, typeCheckLaws)
        
      }
    }
  }
