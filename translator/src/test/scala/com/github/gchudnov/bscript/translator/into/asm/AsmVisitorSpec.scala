package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.builder.{AstMeta, Builder}
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.symbols.{SymbolRef, Type, TypeRef, VectorType}
import com.github.gchudnov.bscript.lang.types.{TypeNames, Types}
import com.github.gchudnov.bscript.rewriter.Rewriter
import com.github.gchudnov.bscript.translator.{TTypeCheckLaws, TestSpec}
import com.github.gchudnov.bscript.translator.into.asm
import com.github.gchudnov.bscript.translator.into.asm.laws.{AsmTranslateLaws, AsmTypeCheckLaws}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.translator.laws.TypeInit
import com.github.gchudnov.bscript.translator.util.FileOps

import java.nio.file.Paths
import java.time.{LocalDate, OffsetDateTime}
import scala.collection.immutable.Seq

final class AsmVisitorSpec extends TestSpec:

  private val typeNames: TypeNames = asm.AsmGlobals.typeNames

  "AsmVisitor" when {

    "unary minus" should {
      /**
       * {{{
       *  -10;
       * }}}
       */
      "translate to asm" in {
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

    "set to nothing" should {
      "set the the correct NA values" in {
        val t = Module(
          VarDecl(TypeRef(typeNames.i32Type), "a", NothingVal()),
          VarDecl(TypeRef(typeNames.i64Type), "b", NothingVal()),
          VarDecl(TypeRef(typeNames.f32Type), "c", NothingVal()),
          VarDecl(TypeRef(typeNames.f64Type), "d", NothingVal()),
          VarDecl(TypeRef(typeNames.dateType), "e", NothingVal()),
          VarDecl(TypeRef(typeNames.datetimeType), "f", NothingVal()),
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual   = s.show()
            val expected =
              """
                |let a: i32 = I32.MIN_VALUE
                |let b: i64 = I64.MIN_VALUE
                |let c: f32 = F32.NaN
                |let d: f64 = F64.NaN
                |let e: Date = Date.parse("1900-01-01")
                |let f: Date = Date.parse("1900-01-01")
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "module" should {
      "render without braces" in {
        val t = Module(
          StructDecl(
            "A",
            List(
              FieldDecl(TypeRef(typeNames.i32Type), "a"),
            )
          ),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A")))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class A {
                |  a: i32
                |}
                |let a: A = {
                |  a: 0
                |}
                |""".stripMargin.trim

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
       *     string[] ss;
       *     float[] fs;
       *   }
       * }}}
       */
      "translate to asm" in {
        val t = StructDecl("X", List(
          FieldDecl(TypeRef(typeNames.i32Type), "x"),
          FieldDecl(TypeRef(typeNames.f64Type), "y"),
          FieldDecl(VectorType(TypeRef(typeNames.strType)), "ss"),
          FieldDecl(VectorType(TypeRef(typeNames.f32Type)), "fs"),
        ))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class X {
                |  x: i32
                |  y: f64
                |  ss: Array<string>
                |  fs: Array<f32>
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "should translate to asm with values" in {
        val t = Module(
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.strType), "s"), FieldDecl(TypeRef("B"), "b"))),
          VarDecl(
            TypeRef("A"),
            "a",
            StructVal(
              TypeRef("A"),
              Map(
                "x" -> IntVal(1),
                "s" -> StrVal("alice"),
                "b" -> StructVal(
                  TypeRef("B"),
                  Map(
                    "y" -> IntVal(2)
                  )
                )
              )
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class B {
                |  y: i32
                |}
                |class A {
                |  x: i32
                |  s: string
                |  b: B
                |}
                |let a: A = {
                |  x: 1,
                |  s: "alice",
                |  b: {
                |      y: 2
                |    }
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "preserve the order fields without explicit init" in {
        val t = Module(
          StructDecl(
            "A",
            List(
              FieldDecl(TypeRef(typeNames.i32Type), "a"),
              FieldDecl(TypeRef(typeNames.i32Type), "b"),
              FieldDecl(TypeRef(typeNames.i32Type), "c"),
              FieldDecl(TypeRef(typeNames.i32Type), "d"),
              FieldDecl(TypeRef(typeNames.i32Type), "e"),
              FieldDecl(TypeRef(typeNames.i32Type), "f"),
              FieldDecl(TypeRef(typeNames.i32Type), "g")
            )
          ),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A")))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class A {
                |  a: i32
                |  b: i32
                |  c: i32
                |  d: i32
                |  e: i32
                |  f: i32
                |  g: i32
                |}
                |let a: A = {
                |  a: 0,
                |  b: 0,
                |  c: 0,
                |  d: 0,
                |  e: 0,
                |  f: 0,
                |  g: 0
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "preserve the order fields during explicit initialization" in {
        val t = Module(
          StructDecl(
            "A",
            List(
              FieldDecl(TypeRef(typeNames.i32Type), "a"),
              FieldDecl(TypeRef(typeNames.i32Type), "b"),
              FieldDecl(TypeRef(typeNames.i32Type), "c"),
              FieldDecl(TypeRef(typeNames.i32Type), "d"),
              FieldDecl(TypeRef(typeNames.i32Type), "e")
            )
          ),
          VarDecl(
            TypeRef("A"),
            "x",
            StructVal(
              TypeRef("A"),
              Map(
                "a" -> IntVal(1),
                "b" -> IntVal(2),
                "c" -> IntVal(3),
                "d" -> IntVal(4),
                "e" -> IntVal(5)
              )
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class A {
                |  a: i32
                |  b: i32
                |  c: i32
                |  d: i32
                |  e: i32
                |}
                |let x: A = {
                |  a: 1,
                |  b: 2,
                |  c: 3,
                |  d: 4,
                |  e: 5
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "block" should {
      "translate to asm" in {
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
                |  let x: i32 = 0
                |  x = 3
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to asm if empty" in {
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
      "translate to asm" in {
        val t = MethodDecl(
          TypeRef(typeNames.i32Type),
          "g",
          List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
          Block(
            Return(Sub(Var(SymbolRef("x")), IntVal(1))),
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """function g(x: i32): i32 {
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
       *     void f(int x) { int y = 1; return g(2*x + y); }
       *     void g(int x) { return (x - 1); }
       *     void main() { return f(3); }
       *     main();
       *   }
       * }}}
       */
      "translate to asm call without arguments" in {
        val t = Module(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(1)),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
              Return(Call(SymbolRef("g"), List(Add(Mul(IntVal(2), Var(SymbolRef("x"))), Var(SymbolRef("y"))))))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Return(Sub(Var(SymbolRef("x")), IntVal(1)))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "main",
            List.empty[ArgDecl],
            Block(
              Return(Call(SymbolRef("f"), List(IntVal(3))))
            )
          ),
          Call(SymbolRef("main"), List.empty[Expr])
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """let x: i32 = 1
                |function f(x: i32): i32 {
                |  let y: i32 = 1
                |  return g((2 * x) + y);
                |}
                |function g(x: i32): i32 {
                |  return (x - 1);
                |}
                |function main(): i32 {
                |  return f(3);
                |}
                |main()
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "if" should {
      "translate to ast without blocks" in {
        val t = If(Less(IntVal(7), IntVal(5)), Add(IntVal(2), IntVal(5)), Some(If(Greater(LongVal(1L), IntVal(2)), Block())))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """if (7 < 5) (2 + 5) else if (1 > 2) {}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to ast with blocks" in {
        val t = If(Less(IntVal(7), IntVal(5)), Block(Add(IntVal(2), IntVal(5))), Some(Block(If(Greater(LongVal(1L), IntVal(2)), Block()))))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """if (7 < 5) {
                |  (2 + 5)
                |} else {
                |  if (1 > 2) {}
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "omit 'else' if there is no code inside" in {
        val t        = If(Less(IntVal(4), IntVal(5)), Block(Add(IntVal(2), IntVal(3))), None) // NOTE: ELSE is empty here
        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """if (4 < 5) {
                |  (2 + 3)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "wrap function call with 1 arg in IF-condition with round brackets" in {
        val t = Module(
          MethodDecl(
            TypeRef(typeNames.boolType),
            "isValid",
            List(ArgDecl(TypeRef(typeNames.boolType), "x")),
            Block(
              Return(BoolVal(false))
            )
          ),
          If(
            Call(SymbolRef("isValid"), List(BoolVal(true))),
            IntVal(1),
            Some(IntVal(0))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """function isValid(x: bool): bool {
                |  return false;
                |}
                |if (isValid(true)) 1 else 0
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "wrap function call with no args in IF-condition with round brackets" in {
        val t = Module(
          MethodDecl(
            TypeRef(typeNames.boolType),
            "isValid",
            List.empty[ArgDecl],
            Block(
              Return(BoolVal(true))
            )
          ),
          If(
            Call(SymbolRef("isValid"), List.empty[Expr]),
            IntVal(1),
            Some(IntVal(0))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """function isValid(): bool {
                |  return true;
                |}
                |if (isValid()) 1 else 0
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "assignment" should {
      "write function call" in {
        val t = Module(
          MethodDecl(
            TypeRef(typeNames.boolType),
            "calc",
            List(ArgDecl(TypeRef(typeNames.dateType), "x"), ArgDecl(TypeRef(typeNames.dateType), "y")),
            Block(
              Return(BoolVal(true))
            )
          ),
          VarDecl(
            TypeRef(typeNames.dateType),
            "a",
            DateVal(LocalDate.parse("2020-01-01"))
          ),
          VarDecl(
            TypeRef(typeNames.boolType),
            "res",
            Call(SymbolRef("calc"), List(Var(SymbolRef("a")), DateVal(LocalDate.parse("2022-03-04"))))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """import { Date} from "date";
                |
                |function calc(x: Date, y: Date): bool {
                |  return true;
                |}
                |let a: Date = Date.parse("2020-01-01")
                |let res: bool = calc(a, Date.parse("2022-03-04"))
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "datetime" should {
      "be parsed" in {
        val t = Module(
          VarDecl(
            TypeRef(typeNames.datetimeType),
            "a",
            DateTimeVal(OffsetDateTime.parse("2024-05-01T21:30:43+00:00"))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """import { Date} from "date";
                |
                |let a: Date = Date.parse("2024-05-01T21:30:43Z")
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "recursive program" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int f(int x) {
       *       if(x > 0) {
       *         return x + f(x - 1);
       *       }
       *       else {
       *         return 0;
       *       }
       *     }
       *
       *     f(4); // 4 + (3 + (2 + (1 + (0)))) = 10
       *   }
       * }}}
       */
      "translate to asm" in {
        val t = Module(
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              If(
                Greater(Var(SymbolRef("x")), IntVal(0)),
                Return(Add(
                  Var(SymbolRef("x")),
                  Call(SymbolRef("f"), List(Sub(Var(SymbolRef("x")), IntVal(1))))
                )),
                Some(Return(IntVal(0)))
              )
            )
          ),
          Call(SymbolRef("f"), List(IntVal(4)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """function f(x: i32): i32 {
                |  if (x > 0) return (x + f(x - 1)); else return 0;
                |}
                |f(4)
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to asm with blocks" in {
        val t = Module(
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              If(
                Greater(Var(SymbolRef("x")), IntVal(0)),
                Block(
                  Return(Add(
                    Var(SymbolRef("x")),
                    Call(SymbolRef("f"), List(Sub(Var(SymbolRef("x")), IntVal(1))))
                  ))
                ),
                Some(Block(Return(IntVal(0))))
              )
            )
          ),
          Call(SymbolRef("f"), List(IntVal(4)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """function f(x: i32): i32 {
                |  if (x > 0) {
                |    return (x + f(x - 1));
                |  } else {
                |    return 0;
                |  }
                |}
                |f(4)
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "collection" should {
      "translate to asm" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(IntVal(1), IntVal(2), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """{
                |  let a: Array<i32> = [1, 2, 3]
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "access" should {
      "translate to asm" in {
        val t = Module(
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          StructDecl("C", List(FieldDecl(TypeRef(typeNames.i32Type), "z"))),
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"), FieldDecl(TypeRef("C"), "c"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          StructDecl("D", List(FieldDecl(TypeRef(typeNames.i32Type), "i"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef("D"), "d", Init(TypeRef("D"))),
              Assign(
                Access(Var(SymbolRef("d")), Var(SymbolRef("i"))),
                Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y")))
              )
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """class B {
                |  y: i32
                |}
                |class C {
                |  z: i32
                |}
                |class A {
                |  x: i32
                |  b: B
                |  c: C
                |}
                |let a: A = {
                |  x: 0,
                |  b: {
                |      y: 0
                |    },
                |  c: {
                |      z: 0
                |    }
                |}
                |class D {
                |  i: i32
                |}
                |function f(): void {
                |  let d: D = {
                |    i: 0
                |  }
                |  d.i = a.b.y
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "call a method" should {
      "translate if the collection-argument is non-empty" in {
        val t = asm.AsmGlobals.prelude ++ Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(4), Vec(List(IntVal(4)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """  function contains(x: i32, xs: Array<i32>): bool {
                |    return xs.includes(x);
                |  }
                |""".stripMargin.trim

            actual.contains(expected) mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }

//      "translate if the collection-argument is empty" in {
//        val t = asm.AsmGlobals.prelude ++ Block(
//          VarDecl(
//            TypeRef(typeNames.boolType),
//            "x",
//            Call(SymbolRef("contains"), List(IntVal(4), Vec())) // TODO: an empty array should be convertable to any array type
//          ),
//          Var(SymbolRef("x"))
//        )
//
//        val errOrRes = eval(t)
//        errOrRes match
//          case Right(s) =>
//            val actual = s.show()
//            val expected =
//              """
//                |var x: Boolean = contains(4, List.empty)
//                |""".stripMargin.trim
//
//            actual.contains(expected) mustBe true
//          case Left(t) =>
//            fail("Should be 'right", t)
//      }
    }

    "function calls" should {
      "be rewritten to include data-types" in {
        val t = asm.AsmGlobals.prelude ++ Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(4), Vec(List(IntVal(4)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = build(t)
          .flatMap(astMeta => {
            val ast1 = astMeta.ast

            // maps function name to the argument index to take the type from for a suffix
            val mapper = Map(
              "contains" -> 0,
              "isDefined" -> 0,
              "round" -> 0,
              "truncate" -> 0,
              "coalesce" -> 0,
              "contains" -> 0,
            )

            val errOrAst2 = Rewriter.map(ast1, {
              case call: Call =>
                val id = call.id.name
                val fnSuffix = mapper.get(id).map(argIdx => call.args(argIdx).evalType.name)
                fnSuffix.map(suffix => call.copy(id = SymbolRef(s"${id}_${suffix}"))).getOrElse(call)
              case a =>
                a
            })

            errOrAst2
          })
          .flatMap(ast2 => eval(ast2))
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """  function contains_int(x: i32, xs: Array<i32>): bool {
                |    return xs.includes(x);
                |  }
                |  let x: bool = contains_int(4, [4])
                |  x
                |""".stripMargin.trim

            actual.contains(expected) mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "compiled expressions" should {
      "translate to asm" in {
        val inits = Inits.codeBlocks(Seq(
          Inits.Keys.NAConstants,
          Inits.Keys.InlineTest,
        ))

        val t = AsmPrelude.make(typeNames)
//            :+
//            Block(
//              Call(SymbolRef("isDefined_string"), List(StrVal("123")))
//            )

        val errOrRes = eval(t, inits)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
//            FileOps.saveString(Paths.get("/home/gchudnov/Projects/wasmdemo/test1.ts"), actual)
//            println(actual)
            val expected =
              """import { Date} from "date";
                |
                |
                |const NAdate: Date = Date.parse("1900-01-01");
                |
                |// isDefined
                |console.log("\n\n# isDefined\n");
                |console.log("isDefined(\"hello world\"): " + isDefined_string("hello world").toString());
                |console.log("isDefined(\"!#\"): " + isDefined_string("!#").toString());
                |console.log("isDefined(123): " + isDefined_int(123).toString());
                |console.log("isDefined(I32.MIN_VALUE): " + isDefined_int(I32.MIN_VALUE).toString());
                |console.log("isDefined(123L): " + isDefined_long(123).toString());
                |console.log("isDefined(I64.MIN_VALUE): " + isDefined_long(I64.MIN_VALUE).toString());
                |console.log("isDefined(123.0f): " + isDefined_float(123.0).toString());
                |console.log("isDefined(F32.NaN): " + isDefined_float(F32.NaN).toString());
                |console.log("isDefined(123.0): " + isDefined_double(123.0).toString());
                |console.log("isDefined(F64.NaN): " + isDefined_double(F64.NaN).toString());
                |console.log("isDefined_date(Date.parse(\"2024-05-01\")): " + isDefined_date(Date.parse("2024-05-01")).toString());
                |console.log("isDefined_date(Date.parse(\"1900-01-01\")): " + isDefined_date(Date.parse("1900-01-01")).toString());
                |console.log("isDefined_datetime(Date.parse(\"2024-05-01T21:30:43+00:00\")): " + isDefined_datetime(Date.parse("2024-05-01T21:30:43+00:00")).toString());
                |console.log("isDefined_datetime(Date.parse(\"1900-01-01\")): " + isDefined_datetime(Date.parse("1900-01-01")).toString());
                |
                |// now
                |console.log("\n\n# now\n");
                |console.log("now(): " + now().toString());
                |
                |// today
                |console.log("\n\n# today\n");
                |console.log("today(): " + today().toString());
                |
                |// round
                |console.log("\n\n# round\n");
                |console.log("round(123.456, 2): " + round_double(123.456, 2).toString());
                |console.log("round(123.444, 2): " + round_double(123.444, 2).toString());
                |console.log("round(123.456f, 2): " + round_float(123.456, 2).toString());
                |console.log("round(123.444f, 2): " + round_float(123.444, 2).toString());
                |
                |// truncate
                |console.log("\n\n# truncate\n");
                |console.log("truncate(123.456, 2): " + truncate_double(123.456, 2).toString());
                |console.log("truncate(123.444, 2): " + truncate_double(123.444, 2).toString());
                |console.log("truncate(123.456f, 2): " + truncate_float(123.456, 2).toString());
                |console.log("truncate(123.444f, 2): " + truncate_float(123.444, 2).toString());
                |
                |// coalesce
                |console.log("\n\n# coalesce\n");
                |console.log("coalesce(\"a\", \"b\"): " + coalesce_string("a", "b").toString());
                |console.log("coalesce(\"!#\", \"b\"): " + coalesce_string("!#", "b").toString());
                |console.log("coalesce(10, 20): " + coalesce_int(10, 20).toString());
                |console.log("coalesce(I32.MIN_VALUE, 20): " + coalesce_int(I32.MIN_VALUE, 20).toString());
                |console.log("coalesce(10L, 20L): " + coalesce_long(10, 20).toString());
                |console.log("coalesce(I64.MIN_VALUE, 20L): " + coalesce_long(I64.MIN_VALUE, 20).toString());
                |console.log("coalesce(10.0f, 20.0f): " + coalesce_float(10.0, 20.0).toString());
                |console.log("coalesce(F32.NaN, 20.0f): " + coalesce_float(F32.NaN, 20.0).toString());
                |console.log("coalesce(10.0, 20.0): " + coalesce_double(10.0, 20.0).toString());
                |console.log("coalesce(F64.NaN, 20.0): " + coalesce_double(F64.NaN, 20.0).toString());
                |console.log("coalesce(Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\")): " + coalesce_date(Date.parse("2024-05-01"), Date.parse("2024-05-02")).toISOString());
                |console.log("coalesce(Date.parse(\"1900-01-01\"), Date.parse(\"2024-05-02\")): " + coalesce_date(Date.parse("1900-01-01"), Date.parse("2024-05-02")).toISOString());
                |console.log("coalesce(Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T22:32:44+00:00\")): " + coalesce_datetime(Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T22:32:44+00:00")).toISOString());
                |console.log("coalesce(Date.parse(\"1900-01-01\"), Date.parse(\"2024-05-02T22:32:44+00:00\")): " + coalesce_datetime(Date.parse("1900-01-01"), Date.parse("2024-05-02T22:32:44+00:00")).toISOString());
                |
                |// contains
                |console.log("\n\n# contains\n");
                |console.log("contains(\"a\", [\"a\", \"b\", \"c\"): " + contains_string("a", ["a", "b", "c"]).toString());
                |console.log("contains(\"d\", [\"a\", \"b\", \"c\"): " + contains_string("d", ["a", "b", "c"]).toString());
                |console.log("contains(1, [1, 2, 3]): " + contains_int(1, [1, 2, 3]).toString());
                |console.log("contains(4, [1, 2, 3]): " + contains_int(4, [1, 2, 3]).toString());
                |console.log("contains(1L, [1L, 2L, 3L]): " + contains_long(1, [1, 2, 3]).toString());
                |console.log("contains(4L, [1L, 2L, 3L]): " + contains_long(4, [1, 2, 3]).toString());
                |console.log("contains(1.0f, [1.0f, 2.0f, 3.0f]): " + contains_float(1.0, [1.0, 2.0, 3.0]).toString());
                |console.log("contains(4.0f, [1.0f, 2.0f, 3.0f]): " + contains_float(4.0, [1.0, 2.0, 3.0]).toString());
                |console.log("contains(1.0, [1.0, 2.0, 3.0]): " + contains_double(1.0, [1.0, 2.0, 3.0]).toString());
                |console.log("contains(4.0, [1.0, 2.0, 3.0]): " + contains_double(4.0, [1.0, 2.0, 3.0]).toString());
                |console.log("contains(Date.parse(\"2024-05-01\"), [Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\"), Date.parse(\"2024-05-03\")]): " + contains_date(Date.parse("2024-05-01"), [Date.parse("2024-05-01"), Date.parse("2024-05-02"), Date.parse("2024-05-03")]).toString());
                |console.log("contains(Date.parse(\"2024-05-04\"), [Date.parse(\"2024-05-01\"), Date.parse(\"2024-05-02\"), Date.parse(\"2024-05-03\")]): " + contains_date(Date.parse("2024-05-04"), [Date.parse("2024-05-01"), Date.parse("2024-05-02"), Date.parse("2024-05-03")]).toString());
                |console.log("contains(Date.parse(\"2024-05-01T21:30:43+00:00\"), [Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T21:30:43+00:00\"), Date.parse(\"2024-05-03T21:30:43+00:00\")]): " + contains_datetime(Date.parse("2024-05-01T21:30:43+00:00"), [Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T21:30:43+00:00"), Date.parse("2024-05-03T21:30:43+00:00")]).toString());
                |console.log("contains(Date.parse(\"2024-05-04T21:30:43+00:00\"), [Date.parse(\"2024-05-01T21:30:43+00:00\"), Date.parse(\"2024-05-02T21:30:43+00:00\"), Date.parse(\"2024-05-03T21:30:43+00:00\")]): " + contains_datetime(Date.parse("2024-05-04T21:30:43+00:00"), [Date.parse("2024-05-01T21:30:43+00:00"), Date.parse("2024-05-02T21:30:43+00:00"), Date.parse("2024-05-03T21:30:43+00:00")]).toString());
                |
                |// fieldOfDateTime
                |console.log("\n\n# fieldOfDateTime\n");
                |console.log("fieldOfDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), \"days\"): " + fieldOfDateTime_datetime(Date.parse("2024-05-01T21:30:43+00:00"), "days").toString());
                |console.log("fieldOfDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), \"hours\"): " + fieldOfDateTime_datetime(Date.parse("2024-05-01T21:30:43+00:00"), "hours").toString());
                |console.log("fieldOfDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), \"minutes\"): " + fieldOfDateTime_datetime(Date.parse("2024-05-01T21:30:43+00:00"), "minutes").toString());
                |console.log("fieldOfDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), \"seconds\"): " + fieldOfDateTime_datetime(Date.parse("2024-05-01T21:30:43+00:00"), "seconds").toString());
                |
                |// setDateTime
                |console.log("\n\n# setDateTime\n");
                |const dt1 = Date.parse("2024-05-01T21:30:43+00:00");
                |setDateTime_datetime(dt1, 5, "days");
                |console.log("setDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 5, \"days\"): " + dt1.toISOString());
                |
                |const dt2 = Date.parse("2024-05-01T21:30:43+00:00");
                |setDateTime_datetime(dt2, 12, "hours");
                |console.log("setDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 12, \"hours\"): " + dt2.toISOString());
                |
                |const dt3 = Date.parse("2024-05-01T21:30:43+00:00");
                |setDateTime_datetime(dt3, 34, "minutes");
                |console.log("setDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 34, \"minutes\"): " + dt3.toISOString());
                |
                |const dt4 = Date.parse("2024-05-01T21:30:43+00:00");
                |setDateTime_datetime(dt4, 56, "seconds");
                |console.log("setDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 56, \"seconds\"): " + dt4.toISOString());
                |
                |// offsetDateTime
                |console.log("\n\n# offsetDateTime\n");
                |const dt11 = Date.parse("2024-05-01T21:30:43+00:00");
                |offsetDateTime_datetime(dt11, 5, "days");
                |console.log("offsetDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 5, \"days\"): " + dt11.toISOString());
                |
                |const dt12 = Date.parse("2024-05-01T21:30:43+00:00");
                |offsetDateTime_datetime(dt12, 50, "days");
                |console.log("offsetDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 50, \"days\"): " + dt12.toISOString());
                |
                |const dt13 = Date.parse("2024-05-01T21:30:43+00:00");
                |offsetDateTime_datetime(dt13, 12, "hours");
                |console.log("offsetDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 12, \"hours\"): " + dt13.toISOString());
                |
                |const dt14 = Date.parse("2024-05-01T21:30:43+00:00");
                |offsetDateTime_datetime(dt14, 34, "minutes");
                |console.log("offsetDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 34, \"minutes\"): " + dt14.toISOString());
                |
                |const dt15 = Date.parse("2024-05-01T21:30:43+00:00");
                |offsetDateTime_datetime(dt15, 56, "seconds");
                |console.log("offsetDateTime(Date.parse(\"2024-05-01T21:30:43+00:00\"), 56, \"seconds\"): " + dt15.toISOString());
                |
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_string(x: string): bool {
                |  return x !== "!#";
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_int(x: i32): bool {
                |  return x !== I32.MIN_VALUE;
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_long(x: i64): bool {
                |  return x !== I64.MIN_VALUE;
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_float(x: f32): bool {
                |  return !F32.isNaN(x);
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_double(x: f64): bool {
                |  return !F64.isNaN(x);
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_date(x: Date): bool {
                |  return x.getTime() !== NAdate.getTime();
                |}
                |/**
                | * returns true of the provided variable is defined, otherwise false
                | * [std]
                | */
                |function isDefined_datetime(x: Date): bool {
                |  return x.getTime() !== NAdate.getTime();
                |}
                |/**
                | * Returns the current date and time
                | * [std]
                | */
                |function now(): Date {
                |  return new Date(wasi_Date.now())
                |}
                |/**
                | * Returns today as date
                | * [std]
                | */
                |function today(): Date {
                |  let dt = new Date(wasi_Date.now())
                |  return Date.parse(dt.toISOString().substring(0, 10))
                |}
                |/**
                | * Rounds the provided value with the given precision
                | * [std]
                | */
                |function round_double(value: f64, precision: i32): f64 {
                |  let s: f64 = Math.pow(<f64>10, <f64>precision)
                |  return Math.round(value * s) / s
                |}
                |/**
                | * Rounds the provided value with the given precision
                | * [std]
                | */
                |function round_float(value: f32, precision: i32): f32 {
                |  let s: f32 = Mathf.pow(<f32>10, <f32>precision)
                |  return Mathf.round(value * s) / s
                |}
                |/**
                | * Truncates the provided value with the given precision
                | * [std]
                | */
                |function truncate_double(value: f64, precision: i32): f64 {
                |  let s: f64 = Math.pow(<f64>10, <f64>precision)
                |  if (value < 0.0) {
                |    return Math.ceil(value * s) / s;
                |  } else {
                |    return Math.floor(value * s) / s;
                |  }
                |}
                |/**
                | * Truncates the provided value with the given precision
                | * [std]
                | */
                |function truncate_float(value: f32, precision: i32): f32 {
                |  let s: f32 = Mathf.pow(<f32>10, <f32>precision)
                |  if (value < 0.0) {
                |    return Mathf.ceil(value * s) / s;
                |  } else {
                |    return Mathf.floor(value * s) / s;
                |  }
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_string(x: string, y: string): string {
                |  if (x !== "!#") {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_int(x: i32, y: i32): i32 {
                |  if (x !== I32.MIN_VALUE) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_long(x: i64, y: i64): i64 {
                |  if (x !== I64.MIN_VALUE) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_float(x: f32, y: f32): f32 {
                |  if (!F32.isNaN(x)) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_double(x: f64, y: f64): f64 {
                |  if (!F64.isNaN(x)) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_date(x: Date, y: Date): Date {
                |  if (x.getTime() !== NAdate.getTime()) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * returns the first non-null value out of two values that were provided
                | * [std]
                | */
                |function coalesce_datetime(x: Date, y: Date): Date {
                |  if (x.getTime() !== NAdate.getTime()) {
                |    return x;
                |  }
                |  return y;
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_string(x: string, xs: Array<string>): bool {
                |  return xs.includes(x);
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_int(x: i32, xs: Array<i32>): bool {
                |  return xs.includes(x);
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_long(x: i64, xs: Array<i64>): bool {
                |  return xs.includes(x);
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_float(x: f32, xs: Array<f32>): bool {
                |  return xs.includes(x);
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_double(x: f64, xs: Array<f64>): bool {
                |  return xs.includes(x);
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_date(x: Date, xs: Array<Date>): bool {
                |  for (let i = 0; i < xs.length; i++) {
                |    if (xs[i].getTime() == x.getTime()) {
                |      return true;
                |    }
                |  }
                |  return false;
                |}
                |/**
                | * Tests whether the collection contains the given element.
                | * [std]
                | */
                |function contains_datetime(x: Date, xs: Array<Date>): bool {
                |  for (let i = 0; i < xs.length; i++) {
                |    if (xs[i].getTime() == x.getTime()) {
                |      return true;
                |    }
                |  }
                |  return false;
                |}
                |/**
                | * Returns the specified field of date-time as an integer value
                | * [std]
                | */
                |function fieldOfDateTime_datetime(value: Date, unit: string): i32 {
                |  if (unit === "days") {
                |    return value.getUTCDate();
                |  } else if (unit === "hours") {
                |    return value.getUTCHours();
                |  } else if (unit === "minutes") {
                |    return value.getUTCMinutes();
                |  } else if (unit === "seconds") {
                |    return value.getUTCSeconds();
                |  }
                |  return -1;
                |}
                |/**
                | * Sets a field of datetime to the specified value
                | * [std]
                | */
                |function setDateTime_datetime(value: Date, offset: i32, unit: string): void {
                |  if (unit === "days") {
                |    value.setUTCDate(offset);
                |  } else if (unit === "hours") {
                |    value.setUTCHours(offset);
                |  } else if (unit === "minutes") {
                |    value.setUTCMinutes(offset);
                |  } else if (unit === "seconds") {
                |    value.setUTCSeconds(offset);
                |  }
                |}
                |/**
                | * Offsets the provided date-time
                | * [std]
                | */
                |function offsetDateTime_datetime(value: Date, offset: i32, unit: string): void {
                |  if (unit === "days") {
                |    value.setUTCDate(value.getUTCDate() + offset);
                |  } else if (unit === "hours") {
                |    value.setUTCHours(value.getUTCHours() + offset);
                |  } else if (unit === "minutes") {
                |    value.setUTCMinutes(value.getUTCMinutes() + offset);
                |  } else if (unit === "seconds") {
                |    value.setUTCSeconds(value.getUTCSeconds() + offset);
                |  }
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  private def eval(ast0: AST, inits: List[(String, Seq[String])] = List.empty[(String, Seq[String])]): Either[Throwable, AsmState] =
    val types = Types.make(typeNames)
    val typeCheckLaws = AsmTypeCheckLaws.make(types)

    Builder
      .build(ast0, types, typeCheckLaws)
      .flatMap(astMeta =>
        val typeInit = AsmTypeInit
        val typeNa = AsmTypeNA
        val laws = AsmTranslateLaws.make(typeNames, typeInit, typeNa, astMeta.meta)

        val cVisitor: AsmVisitor = AsmVisitor.make(laws)
        val cState: AsmState = withInits(AsmState.make(astMeta.meta), inits)

        astMeta.ast.visit(cState, cVisitor)
      )

  private def build(ast0: AST): Either[Throwable, AstMeta] = {
    val types = Types.make(typeNames)
    val typeCheckLaws = AsmTypeCheckLaws.make(types)

    Builder
      .build(ast0, types, typeCheckLaws)
  }

  private def withInits(s: AsmState, inits: List[(String, Seq[String])]): AsmState = {
    s.copy(
      inits = s.inits ++ inits
    )
  }