package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.builder.BTypeCheckLaws

final class BuilderSpec extends TestSpec:
  "Builder" when {
    // "AST is build" should {
    //   "resolve scopes, define symbols and assign types" in {

    //     val typeNames     = BGlobals.typeNames
    //     val types         = Types.make(typeNames)
    //     val typeCheckLaws = BTypeCheckLaws.make(types)

    //     val ast0 = Block(
    //       VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
    //       VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
    //       Var(SymbolRef("y"))
    //     )

    //     val errOrRes = Builder.build(ast0, types, typeCheckLaws)
    //     errOrRes match
    //       case Right(AstMeta(ast1, meta1)) =>
    //         val block    = ast1.asInstanceOf[Block]
    //         val autoDecl = block.statements(0).asInstanceOf[VarDecl]
    //         val varDecl  = block.statements(1).asInstanceOf[VarDecl]
    //         val yVar     = block.statements(2).asInstanceOf[Var]

    //         autoDecl.evalType.name mustBe (typeNames.voidType)
    //         autoDecl.expr.evalType.name mustBe (typeNames.i32Type)
    //         varDecl.vType.name mustBe (typeNames.i32Type)
    //         yVar.evalType.name mustBe (typeNames.i32Type)
    //       case Left(t) =>
    //         fail("Should be 'right", t)
    //   }
    // }
  }
