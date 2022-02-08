package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.builder.internal.MetaOps
import com.github.gchudnov.bscript.builder.BGlobals
import com.github.gchudnov.bscript.lang.ast.CompiledExpr
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.builder.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.lang.util.Show.ShowOps
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitorSpec.dehydrate
import com.github.gchudnov.bscript.lang.util.{ EqWrap, Gen, Transform }
import com.github.gchudnov.bscript.builder.TestSpec

/**
 * ScopeBuildVisitor tests
 */
final class ScopeBuildVisitorSpec extends TestSpec:
  import ScopeBuildVisitorSpec.*
  import Meta.*
  import MetaOps.*

  private val typeNames: TypeNames = BGlobals.typeNames

  "ScopeBuildVisitor" when {

    "var is defined" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "put it in a scope" in {
        val t = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "x").map(_.name) mustBe (Some("#global"))
            meta.show().contains("x") mustBe (true)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "functions" should {

      /**
       * {{{
       *   // globals
       *   int main() {
       *     int x;
       *     x = 3;
       *   }
       * }}}
       */
      "define nested scope" in {
        val t = MethodDecl(
          TypeRef(typeNames.i32Type),
          "main",
          List.empty[ArgDecl],
          Block(
            VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
            Assign(Var(SymbolRef("x")), IntVal(3))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "x").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "main").map(_.name) mustBe (Some("#global"))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *       datetime offsetDateTime(value: datetime, offset: int, unit: str) {
       *         // ...
       *       }
       *
       *       int fieldOfDateTime(value: datetime, unit: str) {
       *         // ...
       *       }
       *   }
       * }}}
       */
      "produce several scopes if there are several method declarations" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.datetimeType),
            "offsetDateTime",
            List(
              ArgDecl(TypeRef(typeNames.datetimeType), "value"),
              ArgDecl(TypeRef(typeNames.i32Type), "offset"),
              ArgDecl(TypeRef(typeNames.strType), "unit")
            ),
            Block(
              CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.i32Type),
            "fieldOfDateTime",
            List(
              ArgDecl(TypeRef(typeNames.datetimeType), "value"),
              ArgDecl(TypeRef(typeNames.strType), "unit")
            ),
            Block(
              CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
            )
          )
        )

        val res      = resourceToString("data/scope-build-decl-2.json").toTry.get
        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            // check that scope has the proper shape
            val actual   = dehydrate(meta.show())
            val expected = dehydrate(res)
            actual mustBe expected

            // check that ast -> scope map is correct
            meta.astScopes.size mustBe (12)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "type-declarations" should {

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = 10;  // int
       *     decltype(x) y = 20;
       *     y;
       *   }
       * }}}
       */
      "be allowed in auto-variable declarations" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            meta.astScopes.size mustBe (7)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = 10;
       *     decltype(x) y = 20;
       *     y;
       *   }
       * }}}
       */
      "be allowed in variable declarations" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            meta.astScopes.size mustBe (7)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     fn round(auto x, int32 n) -> decltype(x) { ... }
       *
       *     float x = 12.3456f;
       *     int p = 2;
       *
       *     auto z = round(x, p);
       *     z;
       *   }
       * }}}
       */
      "be allowed in functions return types" in {
        val t = Block(
          MethodDecl(
            DeclType(Var(SymbolRef("value"))),
            "round",
            List(
              ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
              ArgDecl(TypeRef(typeNames.i32Type), "precision")
            ),
            Block(
              CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
            )
          ),
          VarDecl(TypeRef(typeNames.f32Type), "x", FloatVal(12.3456f)),
          VarDecl(TypeRef(typeNames.i32Type), "p", IntVal(2)),
          VarDecl(TypeRef(typeNames.autoType), "z", Call(SymbolRef("round"), List(Var(SymbolRef("x")), Var(SymbolRef("p"))))),
          Var(SymbolRef("z"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            meta.astScopes.size mustBe (17)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "variables" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int i = 9;
       *     float j;
       *     int k = i+2;
       *   }
       * }}}
       */
      "be set in a scope" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(9)),
          VarDecl(TypeRef(typeNames.f32Type), "j", FloatVal(0.0f)),
          VarDecl(TypeRef(typeNames.i32Type), "k", Add(Var(SymbolRef("i")), IntVal(2)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            meta.astScopes.size mustBe (9)
            findSymbolScope(meta, "i").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "j").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "k").map(_.name) mustBe (Some("#a"))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int[] a = [1, 2, 3];
       *   }
       * }}}
       */
      "set in the scope for collections" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(IntVal(1), IntVal(2), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "a").map(_.name) mustBe (Some("#a"))
          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = nothing;
       *   }
       * }}}
       */
      "allow nothing in declaration" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", NothingVal())
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "x").map(_.name) mustBe (Some("#a"))
          case Left(t) => fail("Should be 'right", t)
      }
    }

    "scopes are defined" should {

      /**
       * {{{
       *   // globals
       *   {
       *     { int z; }       // local scope nested within f's local scope
       *     printf("%d", z); // z is no longer visible; static analysis ERROR!
       *   }
       * }}}
       */
      "assign scope to the Call Variable" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "printf",
            List(
              ArgDecl(TypeRef(typeNames.strType), "format"),
              ArgDecl(TypeRef(typeNames.autoType), "value")
            ),
            Block.empty
          ),
          Block(
            VarDecl(TypeRef(typeNames.i32Type), "z", IntVal(0))
          ),
          Call(SymbolRef("printf"), List(StrVal("%d"), Var(SymbolRef("z")))) // z is no longer visible; Will be an error in Phase #2
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            val blockStatements = ast.asInstanceOf[Block].statements
            val callExpr        = blockStatements.last.asInstanceOf[Call]
            val zArg            = callExpr.args.last
            val zScope          = meta.astScopes(EqWrap(zArg))

            zScope.name mustBe ("#a")
          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       * // globals
       *   {
       *     int x;        // define variable x in global scope
       *     void f() {    // define function f in global scope
       *       int y;      // define variable y in local scope of f
       *       { int i; }  // define variable i in nested local scope
       *       { int j; }  // define variable j in another nested local scope
       *     }
       *     void g() {    // define function g in global scope
       *       int i;      // define variable i in local scope of g
       *     }
       *   }
       * }}}
       */
      "retain scope information for several nested scopes" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(0)),
              Block(
                VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0))
              ),
              Block(
                VarDecl(TypeRef(typeNames.i32Type), "j", IntVal(0))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "x").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "y").map(_.name) mustBe (Some("#b"))
            findSymbolScope(meta, "j").map(_.name) mustBe (Some("#d"))
            // NOTE: `i` exists in 2 scopes, 'c' & 'e'
            findSymbolScopes(meta, "i").map(_.name) must contain theSameElementsAs (List("#c", "#e"))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "a struct is defined" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct B { int y; };
       *     struct C { int z; };
       *     struct A {
       *       int x;
       *       B b;
       *       C c;
       *     };
       *
       *     A a;
       *
       *     void f() {
       *       struct D {
       *         int i;
       *       };
       *
       *       D d;
       *       d.i = a.b.y;
       *     }
       *   }
       * }}}
       */
      "define related symbols in scopes" in {
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
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "b").map(_.name) mustBe (Some("A"))
            findSymbolScope(meta, "c").map(_.name) mustBe (Some("A"))
            findSymbolScope(meta, "x").map(_.name) mustBe (Some("A"))

            findSymbolScope(meta, "y").map(_.name) mustBe (Some("B"))
            findSymbolScope(meta, "z").map(_.name) mustBe (Some("C"))
            findSymbolScope(meta, "i").map(_.name) mustBe (Some("D"))

            findSymbolScope(meta, "A").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "B").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "D").map(_.name) mustBe (Some("#b"))
            findSymbolScope(meta, "a").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "f").map(_.name) mustBe (Some("#a"))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "a program is given" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int x = 1;
       *     void g(int x) { int z = 2; }
       *     void f(int x) { int y = 1; g(2*x); }
       *     int main() { f(3); }
       *     main();
       *   }
       * }}}
       */
      "build scopes" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(1)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "z", IntVal(2))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
              Call(SymbolRef("g"), List(Mul(IntVal(2), Var(SymbolRef("x")))))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
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
          case Right(ScopeBuildVisitorState(ast, meta)) =>
            findSymbolScope(meta, "f").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "g").map(_.name) mustBe (Some("#a"))
            findSymbolScope(meta, "main").map(_.name) mustBe (Some("#a"))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phase 1 only.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   */
  private def eval(ast0: AST): Either[Throwable, ScopeBuildVisitorState] =
    val (initMeta, rootScope) = BGlobals.make()
    val v1                    = ScopeBuildVisitor.make()
    val s1                    = ScopeBuildState.make(ast0, initMeta, rootScope, Gen.empty)

    ast0
      .visit(s1, v1)
      .flatMap { s11 =>
        val t    = ScopeBuildVisitorState(meta = s11.meta, ast = s11.ast)
        val ast1 = s11.ast

        // println(t.meta.show())

        ast1
          .visit(".", verifyDefined(t))
          .map(_ => t)
      }

object ScopeBuildVisitorSpec:

  final case class ScopeBuildVisitorState(ast: AST, meta: Meta)

  def dehydrate(s: String): String =
    s.replaceAll("\\s", "")


  def verifyDefined(t: ScopeBuildVisitorState): TreeVisitor[String, Unit] = new TreeVisitor[String, Unit]:

    override def visit(s: String, n: Init): Either[Throwable, Unit] = for _ <- checkDefined(n, s"${s} -> Init")
    yield ()

    override def visit(s: String, n: UnaryMinus): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "UnaryMinus")
      _ <- n.expr.visit(s"${s} -> UnaryMinus(expr", this)
    yield ()

    override def visit(s: String, n: Add): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Add")
      _ <- n.lhs.visit(s"${s} -> Add(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Add(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Sub): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Sub")
      _ <- n.lhs.visit(s"${s} -> Sub(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Sub(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Mul): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Mul")
      _ <- n.lhs.visit(s"${s} -> Mul(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mul(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Div): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Div")
      _ <- n.lhs.visit(s"${s} -> Div(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Div(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Mod): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Mod")
      _ <- n.lhs.visit(s"${s} -> Mod(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mod(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Less): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Less")
      _ <- n.lhs.visit(s"${s} -> Less(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Less(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: LessEqual): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "LessEqual")
      _ <- n.lhs.visit(s"${s} -> LessEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> LessEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Greater): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Greater")
      _ <- n.lhs.visit(s"${s} -> Greater(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Greater(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: GreaterEqual): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "GreaterEqual")
      _ <- n.lhs.visit(s"${s} -> GreaterEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> GreaterEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Equal): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Equal")
      _ <- n.lhs.visit(s"${s} -> Equal(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Equal(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: NotEqual): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "NotEqual")
      _ <- n.lhs.visit(s"${s} -> NotEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> NotEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Not): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Not")
      _ <- n.expr.visit(s"${s} -> Not(expr", this)
    yield ()

    override def visit(s: String, n: And): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "And")
      _ <- n.lhs.visit(s"${s} -> And(lhs", this)
      _ <- n.rhs.visit(s"${s} -> And(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Or): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Or")
      _ <- n.lhs.visit(s"${s} -> Or(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Or(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Assign): Either[Throwable, Unit] = for
      _ <- checkDefined(n, "Assign")
      _ <- n.id.visit(s"${s} -> Assign(id", this)
      _ <- n.expr.visit(s"${s} -> Assign(id, expr", this)
    yield ()

    override def visit(s: String, n: NothingVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "NothingVal")
    yield ()

    override def visit(s: String, n: VoidVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "VoidVal")
    yield ()

    override def visit(s: String, n: BoolVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "BoolVal")
    yield ()

    override def visit(s: String, n: IntVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "IntVal")
    yield ()

    override def visit(s: String, n: LongVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "LongVal")
    yield ()

    override def visit(s: String, n: FloatVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "FloatVal")
    yield ()

    override def visit(s: String, n: DoubleVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "DoubleVal")
    yield ()

    override def visit(s: String, n: DecimalVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "DecimalVal")
    yield ()

    override def visit(s: String, n: StrVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "StrVal")
    yield ()

    override def visit(s: String, n: DateVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "DateVal")
    yield ()

    override def visit(s: String, n: DateTimeVal): Either[Throwable, Unit] = for _ <- checkDefined(n, "DateTimeVal")
    yield ()

    override def visit(s: String, n: Vec): Either[Throwable, Unit] =
      Transform.sequence(n.elements.zipWithIndex.map { case (expr, i) => expr.visit(s"${s} -> Vec(${i})", this) }).map(_ => ())

    override def visit(s: String, n: Var): Either[Throwable, Unit] = for _ <- checkDefined(n, s"${s} -> Var")
    yield ()

    override def visit(s: String, n: ArgDecl): Either[Throwable, Unit] = for _ <- checkDefined(n, s"${s} -> ArgDecl(${n.symbol.name}")
    yield ()

    override def visit(s: String, n: VarDecl): Either[Throwable, Unit] = for
      _ <- checkDefined(n, s"${s} -> VarDecl(${n.symbol.name}")
      _ <- n.expr.visit(s"${s} -> VarDecl(${n.vType.name} ${n.name} = expr", this)
    yield ()

    override def visit(s: String, n: FieldDecl): Either[Throwable, Unit] = for _ <- checkDefined(n, s"${s} -> FieldDecl(${n.symbol.name}")
    yield ()

    override def visit(s: String, n: MethodDecl): Either[Throwable, Unit] = for
      _ <- checkDefined(n, s"${s} -> MethodDecl(${n.symbol.name}) -> retType")
      _ <- Transform.sequence(n.params.map(p => p.visit(s"${s} -> MethodDecl(${n.name})(${p.name})", this)))
      _ <- n.body.visit(s"${s} -> MethodDecl(${n.name})(...) {", this)
    yield ()

    override def visit(s: String, n: StructDecl): Either[Throwable, Unit] = for
      _ <- Transform.sequence(n.fields.map(p => p.visit(s"${s} -> StructDecl(${n.name}){${p.name}}", this)))
      _ <- checkDefined(n, s"${s} -> StructDecl(${n.symbol.name})")
    yield ()

    override def visit(s: String, n: Block): Either[Throwable, Unit] =
      Transform.sequence(n.statements.map(st => st.visit(s"${s} -> Block {", this))).map(_ => ())

    override def visit(s: String, n: Call): Either[Throwable, Unit] = for
      _ <- checkDefined(n, s"${s} -> Call(${n.id.name})")
      _ <- Transform.sequence(n.args.map(e => e.visit(s"${s} -> Call(${n.id.name})(...)", this)))
    yield ()

    override def visit(s: String, n: If): Either[Throwable, Unit] = for
      _ <- n.cond.visit(s"${s} -> If(...)", this)
      _ <- n.then1.visit(s"${s} -> If(...) Then { ", this)
      _ <- Transform.sequence(n.else1.map(_.visit(s"${s} -> If(...) Then { ... } Else {", this)))
    yield ()

    override def visit(s: String, n: Access): Either[Throwable, Unit] = for
      _ <- n.a.visit(s"${s} -> Access(a", this)
      _ <- n.b.visit(s"${s} -> Access(a, b)", this)
    yield ()

    override def visit(s: String, n: CompiledExpr): Either[Throwable, Unit] = for _ <- checkDefined(n, "CompiledExpr")
    yield ()

    private def checkDefined(n: AST, msg: String): Either[Throwable, Unit] =
      for _ <- Either.cond(t.meta.astScopes.contains(EqWrap(n)), (), new AstException(s"AST [${msg}] was not assigned to the Scope"))
      yield ()
