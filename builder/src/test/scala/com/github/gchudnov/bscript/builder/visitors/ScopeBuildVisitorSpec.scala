package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.builder.TestSpec

/**
 * ScopeBuildVisitorSpec
 */
final class ScopeBuildVisitorSpec extends TestSpec:
  import ScopeBuildVisitorSpec.*

  "ScopeBuildVisitor" when {

    "var is defined" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "put it in a scope" in {
        val t = VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(0)))

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 1
            meta.symbolsByName("x").size mustBe (1)

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
      "define nested scopes" in {
        val t = MethodDecl(
          "main",
          List.empty[TypeDecl],
          List.empty[VarDecl],
          TypeId(TypeName.i32),
          Block.of(
            VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(0))),
            Assign(Id("x"), Literal(IntVal(3)))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 3 // root + main(args) + block inside
            meta.symbolsByName("x").size mustBe (1)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int myFunc(long x) {
       *     int x;
       *     x = 3;
       *   }
       * }}}
       */
      "shadow variables" in {
        val t = MethodDecl(
          "myFunc",
          List.empty[TypeDecl],
          List(VarDecl("x", TypeId(TypeName.i32), Init())),
          TypeId(TypeName.i32),
          Block.of(
            VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(0))),
            Assign(Id("x"), Literal(IntVal(3)))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 3 // root + main(args) + block inside
            meta.symbolsByName("x").size mustBe (2)

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
        val t = Block.of(
          MethodDecl(
            "offsetDateTime",
            List.empty[TypeDecl],
            List(
              VarDecl("value", TypeId(TypeName.datetime), Init()),
              VarDecl("offset", TypeId(TypeName.i32), Init()),
              VarDecl("unit", TypeId(TypeName.str), Init())
            ),
            TypeId(TypeName.datetime),
            Block.of(
              Compiled(callback = Compiled.identity, retType = TypeId(TypeName.datetime))
            )
          ),
          MethodDecl(
            "fieldOfDateTime",
            List.empty[TypeDecl],
            List(
              VarDecl("value", TypeId(TypeName.datetime), Init()),
              VarDecl("unit", TypeId(TypeName.str), Init())
            ),
            TypeId(TypeName.i32),
            Block.of(
              Compiled(callback = Compiled.identity, retType = TypeId(TypeName.i32))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 6
            meta.symbolsByName("offsetDateTime").size mustBe (1)
            meta.symbolsByName("fieldOfDateTime").size mustBe (1)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "type-declarations" should {

      // TODO: do we want to support them in 2.0? or is it worth to add generics, T instead

      //   /**
      //    * {{{
      //    *   // globals
      //    *   {
      //    *     auto x = 10;  // int
      //    *     decltype(x) y = 20;
      //    *     y;
      //    *   }
      //    * }}}
      //    */
      //   "be allowed in auto-variable declarations" in {
      //     val t = Block(
      //       VarDecl(Id(typeNames.autoType), "x", IntVal(10)),
      //       VarDecl(DeclType(Id("x")), "y", IntVal(20)),
      //       Var(SymbolRef("y"))
      //     )

      //     val errOrRes = eval(t)
      //     errOrRes match
      //       case Right(ScopeBuildVisitorState(ast, meta)) =>
      //         meta.astScopes.size mustBe (7)
      //       case Left(t) =>
      //         fail("Should be 'right", t)
      //   }

      //   /**
      //    * {{{
      //    *   // globals
      //    *   {
      //    *     int x = 10;
      //    *     decltype(x) y = 20;
      //    *     y;
      //    *   }
      //    * }}}
      //    */
      //   "be allowed in variable declarations" in {
      //     val t = Block(
      //       VarDecl(TypeId(TypeName.i32), "x", IntVal(10)),
      //       VarDecl(DeclType(Id("x")), "y", IntVal(20)),
      //       Var(SymbolRef("y"))
      //     )

      //     val errOrRes = eval(t)
      //     errOrRes match
      //       case Right(ScopeBuildVisitorState(ast, meta)) =>
      //         meta.astScopes.size mustBe (7)
      //       case Left(t) =>
      //         fail("Should be 'right", t)
      //   }

      //   /**
      //    * {{{
      //    *   // globals
      //    *   {
      //    *     fn round(auto x, int32 n) -> decltype(x) { ... }
      //    *
      //    *     float x = 12.3456f;
      //    *     int p = 2;
      //    *
      //    *     auto z = round(x, p);
      //    *     z;
      //    *   }
      //    * }}}
      //    */
      //   "be allowed in functions return types" in {
      //     val t = Block(
      //       MethodDecl(
      //         DeclType(Var(SymbolRef("value"))),
      //         "round",
      //         List(
      //           VarDecl(Id(typeNames.autoType), "value"), // f32, f64, dec
      //           VarDecl(TypeId(TypeName.i32), "precision")
      //         ),
      //         Block(
      //           CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
      //         )
      //       ),
      //       VarDecl(Id(typeNames.f32Type), "x", FloatVal(12.3456f)),
      //       VarDecl(TypeId(TypeName.i32), "p", IntVal(2)),
      //       VarDecl(Id(typeNames.autoType), "z", Call(SymbolRef("round"), List(Id("x"), Var(SymbolRef("p"))))),
      //       Id("z")
      //     )

      //     val errOrRes = eval(t)
      //     errOrRes match
      //       case Right(ScopeBuildVisitorState(ast, meta)) =>
      //         meta.astScopes.size mustBe (17)
      //       case Left(t) =>
      //         fail("Should be 'right", t)
      //   }

      //   /**
      //    * {{{
      //    *   // globals
      //    *   {
      //    *     fn contains(auto x, decltype(x)[] xs) -> bool { ... }
      //    *
      //    *     bool x = contains(1, []int{ 1, 2, 3 });
      //    *     x;
      //    *   }
      //    * }}}
      //    */
      //   "be allowed in function arguments" in {
      //     val t = Block(
      //       MethodDecl(
      //         Id(typeNames.boolType),
      //         "contains",
      //         List(
      //           VarDecl(Id(typeNames.autoType), "x"),
      //           VarDecl(VectorType(DeclType(Id("x"))), "xs")
      //         ),
      //         Block(
      //           CompiledExpr(callback = CompiledExpr.idCallback, retType = Id(typeNames.boolType))
      //         ),
      //         Seq(ComAnn("Tests whether the list contains the given element."), StdAnn())
      //       ),
      //       VarDecl(
      //         Id(typeNames.boolType),
      //         "x",
      //         Call(SymbolRef("contains"), List(IntVal(1), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
      //       ),
      //       Id("x")
      //     )

      //     val errOrRes = eval(t)
      //     errOrRes match
      //       case Right(ScopeBuildVisitorState(ast, meta)) =>
      //         meta.astScopes.size mustBe (15)
      //       case Left(t) =>
      //         fail("Should be 'right", t)
      //   }

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
        val t = Block.of(
          VarDecl("i", TypeId(TypeName.i32), Literal(IntVal(9))),
          VarDecl("j", TypeId(TypeName.f32), Literal(FloatVal(0.0f))),
          VarDecl("k", TypeId(TypeName.i32), Call(Id("+"), List(Id("i"), Literal(IntVal(2)))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 2
            meta.symbolsByName("i").size mustBe (1)
            meta.symbolsByName("j").size mustBe (1)
            meta.symbolsByName("k").size mustBe (1)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * TODO: NOT CLEAR HOW TO REPRESENT A VECTOR
       * {{{
       *   // globals
       *   {
       *     int[] a = [1, 2, 3];
       *   }
       * }}}
       */
      // "set in the scope for collections" in {
      //   val t = Block.of(
      //     VarDecl(Auto(), "a", Vec(Seq(Literal(IntVal(1)), Literal(IntVal(2)), Literal(IntVal(3)))))
      //   )

      //   val errOrRes = eval(t)
      //   errOrRes match
      //     case Right(State(ast, meta)) =>
      //       meta.forestSize mustBe 2
      //       meta.symbolsByName("a").size mustBe (1)
      //     case Left(t) => fail("Should be 'right", t)
      // }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = nothing;
       *   }
       * }}}
       */
      "allow nothing in declaration" in {
        val t = Block.of(
          VarDecl("x", Auto(), Literal(NullVal()))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forestSize mustBe 2
            meta.symbolsByName("x").size mustBe (1)
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
        val t = Block.of(
          MethodDecl(
            "printf",
            List.empty[TypeDecl],
            List(
              VarDecl("format", TypeId(TypeName.str), Init()),
              VarDecl("value", Auto(), Init())
            ),
            TypeId(TypeName.void),
            Block.empty
          ),
          Block.of(
            VarDecl("z", TypeId(TypeName.i32), Literal(IntVal(0)))
          ),
          Call(Id("printf"), List(Literal(StrVal("%d")), Id("z"))) // z is no longer visible; Will be an error in Phase #2
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            val blockStatements = ast.asInstanceOf[Block].exprs
            val callExpr        = blockStatements.last.asInstanceOf[Call]
            val callScope       = meta.scopeByAST(callExpr)

            callScope.map(_.name) mustBe Some("a.a")
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
        val t = Block.of(
          VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(0))),
          MethodDecl(
            "f",
            List.empty[TypeDecl],
            List.empty[VarDecl],
            TypeId(TypeName.void),
            Block.of(
              VarDecl("y", TypeId(TypeName.i32), Literal(IntVal(0))),
              Block.of(
                VarDecl("i", TypeId(TypeName.i32), Literal(IntVal(0)))
              ),
              Block.of(
                VarDecl("j", TypeId(TypeName.i32), Literal(IntVal(0)))
              )
            )
          ),
          MethodDecl(
            "g",
            List.empty[TypeDecl],
            List.empty[VarDecl],
            TypeId(TypeName.void),
            Block.of(
              VarDecl("i", TypeId(TypeName.i32), Literal(IntVal(0)))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.symbolsByName("x").size mustBe (1)
            meta.symbolsByName("y").size mustBe (1)
            meta.symbolsByName("j").size mustBe (1)

            // NOTE: `i` exists in 2 scopes
            meta.symbolsByName("i").size mustBe (2)

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
        val t = Block.of(
          StructDecl("B", List.empty[TypeDecl], List(VarDecl("y", TypeId(TypeName.i32), Init()))),
          StructDecl("C", List.empty[TypeDecl], List(VarDecl("z", TypeId(TypeName.i32), Init()))),
          StructDecl("A", List.empty[TypeDecl], List(VarDecl("x", TypeId(TypeName.i32), Init()), VarDecl("b", TypeId("B"), Init()), VarDecl("c", TypeId("C"), Init()))),
          VarDecl("a", TypeId("A"), Init()),
          MethodDecl(
            "f",
            List.empty[TypeDecl],
            List.empty[VarDecl],
            TypeId(TypeName.void),
            Block.of(
              StructDecl("D", List(VarDecl("i", TypeId(TypeName.i32), Init()))),
              VarDecl("d", TypeId("D"), Init()),
              Assign(
                Access(Id("d"), Id("i")),
                Access(Access(Id("a"), Id("b")), Id("y"))
              )
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.scopesBySymbol(SymbolRef("b")).map(_.name) must contain theSameElementsAs (List("a.a.c"))
            meta.scopesBySymbol(SymbolRef("c")).map(_.name) must contain theSameElementsAs (List("a.a.c"))
            meta.scopesBySymbol(SymbolRef("x")).map(_.name) must contain theSameElementsAs (List("a.a.c"))

            meta.scopesBySymbol(SymbolRef("y")).map(_.name) must contain theSameElementsAs (List("a.a.a"))
            meta.scopesBySymbol(SymbolRef("z")).map(_.name) must contain theSameElementsAs (List("a.a.b"))
            meta.scopesBySymbol(SymbolRef("i")).map(_.name) must contain theSameElementsAs (List("a.a.d.a.a"))

            meta.scopesBySymbol(SymbolRef("A")).map(_.name) must contain theSameElementsAs (List("a.a"))
            meta.scopesBySymbol(SymbolRef("B")).map(_.name) must contain theSameElementsAs (List("a.a"))
            meta.scopesBySymbol(SymbolRef("D")).map(_.name) must contain theSameElementsAs (List("a.a.d.a"))
            meta.scopesBySymbol(SymbolRef("a")).map(_.name) must contain theSameElementsAs (List("a.a"))
            meta.scopesBySymbol(SymbolRef("f")).map(_.name) must contain theSameElementsAs (List("a.a"))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct B { int y; };
       *
       *     struct A {
       *       int x;
       *       string s;
       *       B b;
       *     };
       *
       *     A a = { 1, "hello", { 2 } };
       *     a
       *   }
       * }}}
       */
      "initialize with an anonymous struct" in {
        val t = Block.of(
          StructDecl("B", List(VarDecl("y", TypeId(TypeName.i32), Init()))),
          StructDecl("A", List(VarDecl("x", TypeId(TypeName.i32), Init()), VarDecl("s", TypeId(TypeName.str), Init()), VarDecl("b", TypeId("B"), Init()))),
          // VarDecl( TODO: THIS CODE SHOULD NOT BE COMMENTED-OUT, CHECK HOW TO INIT A STRUCT
          //   "a",
          //   TypeId("A"),
          //   Literal(
          //     StructVal(
          //       Id("A"),
          //       Map(
          //         "x" -> Literal(IntVal(1)),
          //         "s" -> Literal(StrVal("alice")),
          //         "b" -> Literal(
          //           StructVal(
          //             Id("B"),
          //             Map(
          //               "y" -> Literal(IntVal(2))
          //             )
          //           )
          //         )
          //       )
          //     )
          //   )
          // ),
          Id("a")
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.symbolsByName("a").size mustBe (1)
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
        val t = Block.of(
          VarDecl("x", TypeId(TypeName.i32), Literal(IntVal(1))),
          MethodDecl(
            "g",
            List.empty[TypeDecl],
            List(VarDecl("x", TypeId(TypeName.i32), Init())),
            TypeId(TypeName.void),
            Block.of(
              VarDecl("z", TypeId(TypeName.i32), Literal(IntVal(2)))
            )
          ),
          MethodDecl(
            "f",
            List.empty[TypeDecl],
            List(VarDecl("x", TypeId(TypeName.i32), Init())),
            TypeId(TypeName.void),
            Block.of(
              VarDecl("y", TypeId(TypeName.i32), Literal(IntVal(1))),
              Call(Id("g"), List(Call(Id("*"), List(Literal(IntVal(2)), Id("x")))))
            )
          ),
          MethodDecl(
            "main",
            List.empty[TypeDecl],
            List.empty[VarDecl],
            TypeId(TypeName.void),
            Block.of(
              Call(Id("f"), List(Literal(IntVal(3))))
            )
          ),
          Call(Id("main"), List.empty[Expr])
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.symbolsByName("f").size mustBe (1)
            meta.symbolsByName("g").size mustBe (1)
            meta.symbolsByName("main").size mustBe (1)
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
  private def eval(ast0: AST): Either[Throwable, State] =
    val v1 = ScopeBuildVisitor.make()
    val s1 = ScopeBuilder.make().push() // add the root scope

    val s2 = v1.foldAST(s1, ast0)

    val meta = s2.result

    Right(State(ast0, meta))

object ScopeBuildVisitorSpec:

  final case class State(ast: AST, meta: Meta)

  extension (m: Meta)
    def forestSize: Int = 
      m.forest.size

    def scopeByAST(ast: AST): Option[Scope] =
      m.scopeAsts.scope(ast)

    /**
      * Find all symbols that have the given name
      */
    def symbolsByName(name: String): List[Symbol] =
      m.scopeSymbols.symbolsByName(name)

    /**
     * Find all scopes that contain symbols with the given name
     */
    def scopesBySymbol(sym: SymbolRef): List[Scope] =
      m.scopeSymbols.symbolsByName(sym.name)
        .flatMap(it => m.scopeSymbols.scope(it).map(List(_)).getOrElse(List.empty[Scope]))

