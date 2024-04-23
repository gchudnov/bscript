package com.github.gchudnov.bscript.translator.internal.c

import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.symbols.VectorType
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.translator.TGlobals
import com.github.gchudnov.bscript.translator.TTypeCheckLaws
import com.github.gchudnov.bscript.translator.TestSpec
import com.github.gchudnov.bscript.translator.internal.c.laws.CTranslateLaws
import com.github.gchudnov.bscript.translator.laws.TypeInit

import java.time.LocalDate

final class CVisitorSpec extends TestSpec:

  private val typeNames: TypeNames = TGlobals.typeNames

  "CVisitor" when {

    "unary minus" should {
      /**
       * {{{
       *  -10;
       * }}}
       */
      "translate to c" in {
        val t = UnaryMinus(IntVal(10))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual   = s.show()
            val expected = "-10"

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "struct" should {

      /**
       * {{{
       *   struct X {
       *     int x;
       *     double y;
       *   }
       * }}}
       */
      "translate to c" in {
        val t = StructDecl("X", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.f64Type), "y")))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """struct X {
                |  int32_t x;
                |  double y;
                |};
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "block" should {
      "translate to c" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
          Assign(Var(SymbolRef("x")), IntVal(3))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """{
                |  int32_t x = 0;
                |  x = 3;
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to c if empty" in {
        val t        = Block()
        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """{}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "method" should {
      "translate to c" in {
        val t = MethodDecl(
          TypeRef(typeNames.i32Type),
          "g",
          List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
          Block(
            Sub(Var(SymbolRef("x")), IntVal(1))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """int32_t g(int32_t x) {
                |  return (x - 1);
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = 1;
       *     int f(int x) { int y = 1; return g(2*x + y); }
       *     int g(int x) { return (x - 1); }
       *     int main() { return f(3); }
       *     main();
       *   }
       * }}}
       */
      "translate to c call without arguments" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(1)),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
              Call(SymbolRef("g"), List(Add(Mul(IntVal(2), Var(SymbolRef("x"))), Var(SymbolRef("y")))))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Sub(Var(SymbolRef("x")), IntVal(1))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "main",
            List.empty[ArgDecl],
            Block(
              Call(SymbolRef("f"), List(IntVal(3)))
            )
          ),
          Call(SymbolRef("main"), List.empty[Expr])
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """
                |{
                |  int32_t x = 1;
                |  int32_t f(int32_t x) {
                |    int32_t y = 1;
                |    g((2 * x) + y);
                |  }
                |  int32_t g(int32_t x) {
                |    (x - 1);
                |  }
                |  int32_t main() {
                |    f(3);
                |  }
                |  main();
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

  }

  private def eval(ast0: AST): Either[Throwable, CState] =
    val types = Types.make(typeNames)
    val typeCheckLaws = TTypeCheckLaws.make(types)

    Builder
      .build(ast0, types, typeCheckLaws)
      .flatMap(astMeta =>
        val typeInit = CTypeInit
        val laws = CTranslateLaws.make(typeNames, typeInit, astMeta.meta)

        val cVisitor: CVisitor = CVisitor.make(laws)
        val cState: CState = CState.make(astMeta.meta)

        astMeta.ast.visit(cState, cVisitor)
      )
