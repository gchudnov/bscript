package com.github.gchudnov.bscript.translator.internal.asm

import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.symbols.{SymbolRef, Type, TypeRef, VectorType}
import com.github.gchudnov.bscript.lang.types.{TypeNames, Types}
import com.github.gchudnov.bscript.translator.{TTypeCheckLaws, TestSpec}
import com.github.gchudnov.bscript.translator.internal.asm
import com.github.gchudnov.bscript.translator.internal.asm.laws.{AsmTranslateLaws, AsmTypeCheckLaws}
import com.github.gchudnov.bscript.translator.laws.TypeInit

import java.time.LocalDate

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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
            println(actual)
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
        val t = Block(
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          StructDecl("C", List(FieldDecl(TypeRef(typeNames.i32Type), "z"))),
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"), FieldDecl(TypeRef("C"), "c"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              StructDecl("D", List(FieldDecl(TypeRef(typeNames.i32Type), "i"))),
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
            println(actual)
            val expected =
              """{
                |  struct B {
                |    int32_t y;
                |  };
                |  struct C {
                |    int32_t z;
                |  };
                |  struct A {
                |    int32_t x;
                |    B b;
                |    C c;
                |  };
                |  A a = {
                |    .x = 0,
                |    .b = {
                |        .y = 0
                |      },
                |    .c = {
                |        .z = 0
                |      }
                |  };
                |  void f() {
                |    struct D {
                |      int32_t i;
                |    };
                |    D d = {
                |      .i = 0
                |    };
                |    d.i = a.b.y;
                |  };
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
            Call(SymbolRef("contains"), List(IntVal(4), Vec(List(IntVal(4))))) // TODO: an empty array should be convertable to any array type
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            println(actual)
            // TODO: need to rewrite so that the size of the array is provided
            val expected =
              """
                |  int contains(int32_t x, int32_t xs[]) {
                |    for (int i = 0; i < size; i++) {
                |          if (arr[i] == value) {
                |              return 1; // Return true if the value is found in the array
                |          }
                |      }
                |      return 0; // Return false if the value is not found in the array;
                |  }
                |  int x = contains(4, {4});
                |  x;
                |""".stripMargin.trim

            actual.contains(expected) mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate if the collection-argument is empty" in {
        val t = asm.AsmGlobals.prelude ++ Block(
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(4), Vec())) // TODO: an empty array should be convertable to any array type
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            println(actual)
            val expected =
              """
                |var x: Boolean = contains(4, List.empty)
                |""".stripMargin.trim

            actual.contains(expected) mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "compiled expressions" should {
      "translate to asm" in {
        val t = asm.AsmGlobals.prelude ++ Block(
          VarDecl(TypeRef(typeNames.strType), "s", StrVal("str")),
          Call(SymbolRef("strLen"), List(Var(SymbolRef("s"))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            println(actual)
            val expected =
              """{
                |  /**
                |   * prints the formatted string to StdOut
                |   * [std]
                |   */
                |  def printf(format: String, value: T): Unit = {}
                |  /**
                |   * returns the length of the provided string
                |   * [std]
                |   */
                |  def strLen(s: String): Int = {
                |    s.length
                |  }
                |  /**
                |   * offsets the provided date-time
                |   * [std]
                |   */
                |  def offsetDateTime(value: OffsetDateTime, offset: Int, unit: String): OffsetDateTime = {
                |    unit.trim.toLowerCase match {
                |      case `unitDays` =>
                |        value.plusDays(offset.toLong)
                |      case `unitHours` =>
                |        value.plusHours(offset.toLong)
                |      case `unitMinutes` =>
                |        value.plusMinutes(offset.toLong)
                |      case `unitSeconds` =>
                |        value.plusSeconds(offset.toLong)
                |      case other =>
                |        throw new RuntimeException(s"Unexpected unit of time was passed to offsetDateTime: ${unit}")
                |    }
                |  }
                |  /**
                |   * sets data and time to the specified value
                |   * [std]
                |   */
                |  def setDateTime(value: OffsetDateTime, offset: Int, unit: String): OffsetDateTime = {
                |    unit.trim.toLowerCase match {
                |      case `unitDays` =>
                |        value.withDayOfMonth(offset)
                |      case `unitHours` =>
                |        value.withHour(offset)
                |      case `unitMinutes` =>
                |        value.withMinute(offset)
                |      case `unitSeconds` =>
                |        value.withSecond(offset)
                |      case other =>
                |        throw new RuntimeException(s"Unexpected unit of time was passed to setDateTime: ${unit}")
                |    }
                |  }
                |  /**
                |   * return the specified part of date-time as an integer value
                |   * [std]
                |   */
                |  def fieldOfDateTime(value: OffsetDateTime, unit: String): Int = {
                |    unit.trim.toLowerCase match {
                |      case `unitDays` =>
                |        value.getDayOfMonth
                |      case `unitHours` =>
                |        value.getHour
                |      case `unitMinutes` =>
                |        value.getMinute
                |      case `unitSeconds` =>
                |        value.getSecond
                |      case other =>
                |        throw new RuntimeException(s"Unexpected unit of time was passed to fieldOfDateTime: ${unit}")
                |    }
                |  }
                |  /**
                |   * returns true of the provided variable is defined, otherwise false
                |   * [std]
                |   */
                |  def isDefined(x: T): Boolean = {
                |    x match {
                |      case null => false
                |      case None => false
                |      case _ => true
                |    }
                |  }
                |  /**
                |   * returns the first non-null value out of two values that were provided
                |   * [std]
                |   */
                |  def coalesce(x: T, y: T): T = {
                |    (x, y) match {
                |      case (null, _) => y
                |      case (None, _) => y
                |      case _ => x
                |    }
                |  }
                |  /**
                |   * returns today as date
                |   * [std]
                |   */
                |  def today(): LocalDate = {
                |    LocalDate.now(ZoneId.of("Z"))
                |  }
                |  /**
                |   * returns current date and time as date-time
                |   * [std]
                |   */
                |  def now(): OffsetDateTime = {
                |    OffsetDateTime.now(ZoneId.of("Z"))
                |  }
                |  /**
                |   * rounds the provided value with the given precision
                |   * [std]
                |   */
                |  def round(value: T, precision: Int): T = {
                |    value.setScale(precision, BigDecimal.RoundingMode.HALF_UP)
                |  }
                |  /**
                |   * truncates the provided value with the given precision
                |   * [std]
                |   */
                |  def truncate(value: T, precision: Int): T = {
                |    value.setScale(precision, BigDecimal.RoundingMode.DOWN)
                |  }
                |  /**
                |   * Tests whether the collection contains the given element
                |   * [std]
                |   */
                |  def contains(x: T, xs: List[T]): Boolean = {
                |    // NOTE: Add [T] to the method
                |    xs.contains(x)
                |  }
                |  var s: String = "str"
                |  strLen(s)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

  }

  private def eval(ast0: AST): Either[Throwable, AsmState] =
    val types = Types.make(typeNames)
    val typeCheckLaws = AsmTypeCheckLaws.make(types)

    Builder
      .build(ast0, types, typeCheckLaws)
      .flatMap(astMeta =>
        val typeInit = AsmTypeInit
        val laws = AsmTranslateLaws.make(typeNames, typeInit, astMeta.meta)

        val cVisitor: AsmVisitor = AsmVisitor.make(laws)
        val cState: AsmState = AsmState.make(astMeta.meta)

        astMeta.ast.visit(cState, cVisitor)
      )
