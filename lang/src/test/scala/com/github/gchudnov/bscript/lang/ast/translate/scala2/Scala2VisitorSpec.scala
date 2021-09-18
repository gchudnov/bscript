package com.github.gchudnov.bscript.lang.ast.translate.scala2

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.translate.scala2.Scala2Visitor.*
import com.github.gchudnov.bscript.lang.ast.visitors.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.lang.ast.visitors.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.lang.ast.visitors.TypeCheckVisitor.TypeCheckState
import com.github.gchudnov.bscript.lang.ast.visitors.*
import com.github.gchudnov.bscript.lang.symbols.{ SymbolRef, TypeRef, VectorType }
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.lang.util.Gen
import com.github.gchudnov.bscript.lang.{ TestSpec, Topology }

final class Scala2VisitorSpec extends TestSpec:

  private val typeNames: TypeNames = Globals.typeNames

  "Scala2Visitor" when {

    "unary minus" should {

      /**
       * {{{
       *  -10;
       * }}}
       */
      "translate to code" in {
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
      "translate to code" in {
        val t = StructDecl("X", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.f64Type), "y")))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """final case class X(
                |  var x: Int,
                |  var y: Double
                |)
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "block" should {
      "translate to code" in {
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
                |  var x: Int = 0
                |  x = 3
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to code if empty" in {
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
      "translate to code" in {
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
              """def g(x: Int): Int = {
                |  (x - 1)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "if" should {
      "translate to code without blocks" in {
        val t = If(Less(IntVal(7), IntVal(5)), Add(IntVal(2), IntVal(5)), Some(If(Greater(LongVal(1L), IntVal(2)), Block())))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """if (7 < 5) (2 + 5) else if (1L > 2) {}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to code with blocks" in {
        val t = If(Less(IntVal(7), IntVal(5)), Block(Add(IntVal(2), IntVal(5))), Some(Block(If(Greater(LongVal(1L), IntVal(2)), Block()))))

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """if (7 < 5) {
                |  (2 + 5)
                |} else {
                |  if (1L > 2) {}
                |}
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
      "translate to code" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              If(
                Greater(Var(SymbolRef("x")), IntVal(0)),
                Add(
                  Var(SymbolRef("x")),
                  Call(SymbolRef("f"), List(Sub(Var(SymbolRef("x")), IntVal(1))))
                ),
                Some(IntVal(0))
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
              """{
                |  def f(x: Int): Int = {
                |    if (x > 0) (x + f(x - 1)) else 0
                |  }
                |  f(4)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "translate to code with blocks" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              If(
                Greater(Var(SymbolRef("x")), IntVal(0)),
                Block(
                  Add(
                    Var(SymbolRef("x")),
                    Call(SymbolRef("f"), List(Sub(Var(SymbolRef("x")), IntVal(1))))
                  )
                ),
                Some(Block(IntVal(0)))
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
              """{
                |  def f(x: Int): Int = {
                |    if (x > 0) {
                |      (x + f(x - 1))
                |    } else {
                |      0
                |    }
                |  }
                |  f(4)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "collection" should {
      "translate to code" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(IntVal(1), IntVal(2), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
            val expected =
              """{
                |  var a: List[Int] = List(1, 2, 3)
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "access" should {
      "translate to code" in {
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
            val expected =
              """{
                |  final case class B(
                |    var y: Int
                |  )
                |  final case class C(
                |    var z: Int
                |  )
                |  final case class A(
                |    var x: Int,
                |    var b: B,
                |    var c: C
                |  )
                |  var a: A = A(
                |    x = 0,
                |    b = B(
                |        y = 0
                |      ),
                |    c = C(
                |        z = 0
                |      )
                |  )
                |  def f(): Unit = {
                |    final case class D(
                |      var i: Int
                |    )
                |    var d: D = D(
                |      i = 0
                |    )
                |    d.i = a.b.y
                |  }
                |}
                |""".stripMargin.trim

            actual mustBe expected
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "compiled expressions" should {
      "translate to code" in {
        val t = Globals.prelude ++ Block(
          VarDecl(TypeRef(typeNames.strType), "s", StrVal("str")),
          Call(SymbolRef("strLen"), List(Var(SymbolRef("s"))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(s) =>
            val actual = s.show()
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

  private def eval(ast0: AST): Either[Throwable, Scala2State] =
    val (initMeta, rootScope) = Globals.make()
    val v1                    = ScopeBuildVisitor.make()
    val s1                    = ScopeBuildState.make(ast0, initMeta, rootScope, Gen.empty)

    ast0
      .visit(s1, v1)
      .flatMap { s11 =>
        val v2   = ScopeResolveVisitor.make(typeNames)
        val s2   = ScopeResolveState.make(s11.ast, s11.meta)
        val ast1 = s11.ast

        ast1
          .visit(s2, v2)
          .flatMap { s21 =>
            val ss2  = s21.meta
            val ast2 = s21.ast

            // { AST->Scope } size must be the same before and after Phase #2
            s11.meta.astScopes.size mustEqual (s21.meta.astScopes.size)

            Types
              .make(ss2, typeNames)
              .flatMap { types =>
                val typeCheckTables = TypeCheckTables.make(types)

                val v3 = TypeCheckVisitor.make(types, typeCheckTables)
                val s3 = TypeCheckState.make(ast2, s21.meta)
                ast2
                  .visit(s3, v3)
                  .flatMap({ s31 =>
                    val t    = Topology(meta = s31.meta, ast = s31.ast)
                    val ast3 = s31.ast

                    // { AST->Scope } size must be the same before and after Phase #3
                    s11.meta.astScopes.size mustEqual (s31.meta.astScopes.size)

                    val v4 = Scala2Visitor.make(typeNames, s31.meta)
                    val s4 = Scala2State.make()

                    ast3.visit(s4, v4)
                  })
              }
          }
      }
