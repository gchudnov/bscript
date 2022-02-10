package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.builder.AstMeta
import com.github.gchudnov.bscript.interpreter.memory.IntCell

final class B1Spec extends TestSpec:

  "B1" when {
    val typeNames = B1.typeNames

    val astA = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))
    val astB = Block(
      VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
      VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
      Var(SymbolRef("y"))
    )

    "AST is deserialized" should {
      "produce AST" in {
        val input = resourceToString("data/var-decl-ast.json").toTry.get

        val expected = astA

        val errOrRes = B1.load(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is serialized" should {
      "produce JSON" in {
        val t = astA

        val expected = resourceToString("data/var-decl-ast.json").toTry.get

        val errOrRes = B1.save(t)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is built" should {
      "produce a new AST" in {
        val ast0 = astB

        val errOrRes = B1.build(ast0, B1Options.default.withPrelude(false))
        errOrRes match
          case Right(AstMeta(ast1, meta1)) =>
            val block    = ast1.asInstanceOf[Block]
            val autoDecl = block.statements(0).asInstanceOf[VarDecl]
            val varDecl  = block.statements(1).asInstanceOf[VarDecl]
            val yVar     = block.statements(2).asInstanceOf[Var]

            autoDecl.evalType.name mustBe (typeNames.voidType)
            autoDecl.expr.evalType.name mustBe (typeNames.i32Type)
            varDecl.vType.name mustBe (typeNames.i32Type)
            yVar.evalType.name mustBe (typeNames.i32Type)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is interpreter" should {
      "produce the result" in {
        val ast0 = astB

        val errOrRes = B1.build(ast0).flatMap(B1.interpret)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(20)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is run" should {
      "build & interpret it" in {
        val ast0 = astB

        val errOrRes = B1.run(ast0)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(20)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is translated" should {
      "produce Scala code without prelude" in {
        val ast0 = astB

        val errOrRes = B1.translate(ast0, B1Options.default.withPrelude(false))
        errOrRes match
          case Right(code) =>
            code mustBe """{
                          |  var x: Int = 10
                          |  var y: Int = 20
                          |  y
                          |}""".stripMargin
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "produce Scala code with prelude" in {
        val ast0 = astB

        val errOrRes = B1.translate(ast0)
        errOrRes match
          case Right(code) =>
            println(code)
            code.contains("var y: Int = 20") mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
