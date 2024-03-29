package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef, VectorType }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.translator.TGlobals
import com.github.gchudnov.bscript.translator.TTypeCheckLaws
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3Translator
import com.github.gchudnov.bscript.translator.internal.scala3j.Scala3JTranslator

final class TranslatorSpec extends TestSpec:
  "Translator" when {
    val typeNames = TGlobals.typeNames

    val ast0 = StructDecl("X", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.f64Type), "y")))

    val types         = Types.make(typeNames)
    val typeCheckLaws = TTypeCheckLaws.make(types)

    "translated to Scala" should {

      "produce code" in {
        val errOrRes = Builder
          .build(ast0, types, typeCheckLaws)
          .flatMap(astMeta =>
            val translator = Scala3Translator.make(astMeta.meta, typeNames)
            translator.fromAST(astMeta.ast)
          )

        val expected =
          """final case class X(
            |  var x: Int,
            |  var y: Double
            |)""".stripMargin

        errOrRes match
          case Right(actual) =>
            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "translated to Scala with Java Types" should {
      "produce code" in {
        val errOrRes = Builder
          .build(ast0, types, typeCheckLaws)
          .flatMap(astMeta =>
            val translator = Scala3JTranslator.make(astMeta.meta, typeNames)
            translator.fromAST(astMeta.ast)
          )

        val expected =
          """final case class X(
            |  var x: JInteger,
            |  var y: JDouble
            |)""".stripMargin

        errOrRes match
          case Right(actual) =>
            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "translated from Scala3" should {}

    "translated from Scala3 AST" should {
      "produce AST" in {
        val errOrRes = Builder
          .build(ast0, types, typeCheckLaws)
          .flatMap(astMeta =>
            val translator = Scala3JTranslator.make(astMeta.meta, typeNames)

            val x = 10

            Right(translator.toAST({
              var a = 1
              if (x == 10) then a = 2 else a = 3
            }))
          )

        val expected = Block(
          VarDecl(TypeRef("auto"), "a", IntVal(1)),
          If(
            Call(SymbolRef("=="), List(Var(SymbolRef("x")), IntVal(10))),
            Assign(Var(SymbolRef("a")), IntVal(2)),
            Some(Assign(Var(SymbolRef("a")), IntVal(3)))
          )
        )

        errOrRes match
          case Right(actual) =>
            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
