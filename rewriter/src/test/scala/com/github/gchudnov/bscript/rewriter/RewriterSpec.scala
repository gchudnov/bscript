package com.github.gchudnov.bscript.rewriter

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.rewriter.internal.RTypeNames
import com.github.gchudnov.bscript.rewriter.TestSpec

final class RewriterSpec extends TestSpec:
  "Rewriter" when {
    val typeNames = RTypeNames.make()

    "filter" should {
      "keep only AST nodes that match the predicate" in {
        val ast0 = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))
        )

        val pred = (n: AST) =>
          n match
            case x: VarDecl => false
            case _          => true

        val expected = Block()

        val errOrRes = Rewriter.filter(ast0, pred)
        errOrRes match
          case Right(optAst) =>
            optAst match
              case Some(actual) =>
                actual mustBe (expected)
              case None =>
                fail("Expected Some(AST), got None")
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "map" should {
      "map AST nodes" in {
        val ast0 = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))
        )

        val f = (n: AST) =>
          n match
            case x: VarDecl =>
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(100))
            case x =>
              x

        val expected = Block(
          VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(100))
        )

        val errOrRes = Rewriter.map(ast0, f)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
