package com.github.gchudnov.bscript.builder.visitors

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.builder.util.Ptr
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
        val t = VarDecl(TypeRef.i32, "x", Literal(IntVal(0)))

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 1
            meta.findSymbolsByName("x").size mustBe(1)

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
          TypeRef.i32,
          "main",
          List.empty[ArgDecl],
          Block.of(
            VarDecl(TypeRef.i32, "x", Literal(IntVal(0))),
            Assign(Var(SymbolRef("x")), Literal(IntVal(3)))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 3 // root + main(args) + block inside
            meta.findSymbolsByName("x").size mustBe(1)

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
          TypeRef.i32,
          "myFunc",
          List(ArgDecl(TypeRef.i64, "x")),
          Block.of(
            VarDecl(TypeRef.i32, "x", Literal(IntVal(0))),
            Assign(Var(SymbolRef("x")), Literal(IntVal(3)))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 3 // root + main(args) + block inside
            meta.findSymbolsByName("x").size mustBe(2)

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
            TypeRef.datetime,
            "offsetDateTime",
            List(
              ArgDecl(TypeRef.datetime, "value"),
              ArgDecl(TypeRef.i32, "offset"),
              ArgDecl(TypeRef.str, "unit")
            ),
            Block.of(
              Compiled(callback = Compiled.identity, retType = TypeRef.datetime)
            )
          ),
          MethodDecl(
            TypeRef.i32,
            "fieldOfDateTime",
            List(
              ArgDecl(TypeRef.datetime, "value"),
              ArgDecl(TypeRef.str, "unit")
            ),
            Block.of(
              Compiled(callback = Compiled.identity, retType = TypeRef.i32)
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 6
            meta.findSymbolsByName("offsetDateTime").size mustBe(1)
            meta.findSymbolsByName("fieldOfDateTime").size mustBe(1)

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
    //       VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
    //       VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
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
    //       VarDecl(TypeRef.i32, "x", IntVal(10)),
    //       VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
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
    //           ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
    //           ArgDecl(TypeRef.i32, "precision")
    //         ),
    //         Block(
    //           CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
    //         )
    //       ),
    //       VarDecl(TypeRef(typeNames.f32Type), "x", FloatVal(12.3456f)),
    //       VarDecl(TypeRef.i32, "p", IntVal(2)),
    //       VarDecl(TypeRef(typeNames.autoType), "z", Call(SymbolRef("round"), List(Var(SymbolRef("x")), Var(SymbolRef("p"))))),
    //       Var(SymbolRef("z"))
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
    //         TypeRef(typeNames.boolType),
    //         "contains",
    //         List(
    //           ArgDecl(TypeRef(typeNames.autoType), "x"),
    //           ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
    //         ),
    //         Block(
    //           CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.boolType))
    //         ),
    //         Seq(ComAnn("Tests whether the list contains the given element."), StdAnn())
    //       ),
    //       VarDecl(
    //         TypeRef(typeNames.boolType),
    //         "x",
    //         Call(SymbolRef("contains"), List(IntVal(1), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
    //       ),
    //       Var(SymbolRef("x"))
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
          VarDecl(TypeRef.i32, "i", Literal(IntVal(9))),
          VarDecl(TypeRef.f32, "j", Literal(FloatVal(0.0f))),
          VarDecl(TypeRef.i32, "k", Call(SymbolRef("+"), List(Var(SymbolRef("i")), Literal(IntVal(2)))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 2
            meta.findSymbolsByName("i").size mustBe(1)
            meta.findSymbolsByName("j").size mustBe(1)
            meta.findSymbolsByName("k").size mustBe(1)

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
        val t = Block.of(
          VarDecl(TypeRef.auto, "a", Vec(Seq(Literal(IntVal(1)), Literal(IntVal(2)), Literal(IntVal(3)))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 2
            meta.findSymbolsByName("a").size mustBe(1)
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
        val t = Block.of(
          VarDecl(TypeRef.auto, "x", Literal(NothingVal()))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.forest.size mustBe 2
            meta.findSymbolsByName("x").size mustBe(1)
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
            TypeRef.void,
            "printf",
            List(
              ArgDecl(TypeRef.str, "format"),
              ArgDecl(TypeRef.auto, "value")
            ),
            Block.empty
          ),
          Block.of(
            VarDecl(TypeRef.i32, "z", Literal(IntVal(0)))
          ),
          Call(SymbolRef("printf"), List(Literal(StrVal("%d")), Var(SymbolRef("z")))) // z is no longer visible; Will be an error in Phase #2
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            
            val blockStatements = ast.asInstanceOf[Block].statements
            val callExpr        = blockStatements.last.asInstanceOf[Call]
            val callScope       = meta.scopeAsts.valueKey(Ptr(callExpr))

            callScope.name mustBe ("a.a")
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
          VarDecl(TypeRef.i32, "x", Literal(IntVal(0))),
          MethodDecl(
            TypeRef.void,
            "f",
            List.empty[ArgDecl],
            Block.of(
              VarDecl(TypeRef.i32, "y", Literal(IntVal(0))),
              Block.of(
                VarDecl(TypeRef.i32, "i", Literal(IntVal(0)))
              ),
              Block.of(
                VarDecl(TypeRef.i32, "j", Literal(IntVal(0)))
              )
            )
          ),
          MethodDecl(
            TypeRef.void,
            "g",
            List.empty[ArgDecl],
            Block.of(
              VarDecl(TypeRef.i32, "i", Literal(IntVal(0)))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.findSymbolsByName("x").size mustBe(1)
            meta.findSymbolsByName("y").size mustBe(1)
            meta.findSymbolsByName("j").size mustBe(1)

            // NOTE: `i` exists in 2 scopes
            meta.findSymbolsByName("i").size mustBe(2)

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
          StructDecl("B", List(FieldDecl(TypeRef.i32, "y"))),
          StructDecl("C", List(FieldDecl(TypeRef.i32, "z"))),
          StructDecl("A", List(FieldDecl(TypeRef.i32, "x"), FieldDecl(TypeRef("B"), "b"), FieldDecl(TypeRef("C"), "c"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef.void,
            "f",
            List.empty[ArgDecl],
            Block.of(
              StructDecl("D", List(FieldDecl(TypeRef.i32, "i"))),
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
          case Right(State(ast, meta)) =>
            meta.findSymbolsByName("b").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.c"))
            meta.findSymbolsByName("c").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.c"))
            meta.findSymbolsByName("x").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.c"))

            meta.findSymbolsByName("y").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.a"))
            meta.findSymbolsByName("z").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.b"))
            meta.findSymbolsByName("i").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.d.a.a"))

            meta.findSymbolsByName("A").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a"))
            meta.findSymbolsByName("B").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a"))
            meta.findSymbolsByName("D").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a.d.a"))
            meta.findSymbolsByName("a").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a"))
            meta.findSymbolsByName("f").map(it => meta.scopeSymbols.valueKey(Ptr(it)).name) must contain theSameElementsAs(List("a.a"))

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
          StructDecl("B", List(FieldDecl(TypeRef.i32, "y"))),
          StructDecl("A", List(FieldDecl(TypeRef.i32, "x"), FieldDecl(TypeRef.str, "s"), FieldDecl(TypeRef("B"), "b"))),
          VarDecl(
            TypeRef("A"),
            "a",
            Literal(StructVal(
              TypeRef("A"),
              Map(
                "x" -> Literal(IntVal(1)),
                "s" -> Literal(StrVal("alice")),
                "b" -> Literal(StructVal(
                  TypeRef("B"),
                  Map(
                    "y" -> Literal(IntVal(2))
                  )
                ))
              )
            ))
          ),
          Var(SymbolRef("a"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.findSymbolsByName("a").size mustBe(1)
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
          VarDecl(TypeRef.i32, "x", Literal(IntVal(1))),
          MethodDecl(
            TypeRef.void,
            "g",
            List(ArgDecl(TypeRef.i32, "x")),
            Block.of(
              VarDecl(TypeRef.i32, "z", Literal(IntVal(2)))
            )
          ),
          MethodDecl(
            TypeRef.void,
            "f",
            List(ArgDecl(TypeRef.i32, "x")),
            Block.of(
              VarDecl(TypeRef.i32, "y", Literal(IntVal(1))),
              Call(SymbolRef("g"), List(Call(SymbolRef("*"), List(Literal(IntVal(2)), Var(SymbolRef("x"))))))
            )
          ),
          MethodDecl(
            TypeRef.void,
            "main",
            List.empty[ArgDecl],
            Block.of(
              Call(SymbolRef("f"), List(Literal(IntVal(3))))
            )
          ),
          Call(SymbolRef("main"), List.empty[Expr])
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(State(ast, meta)) =>
            meta.findSymbolsByName("f").size mustBe(1)
            meta.findSymbolsByName("g").size mustBe(1)
            meta.findSymbolsByName("main").size mustBe(1)
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
    val v1                    = ScopeBuildVisitor.make()
    val s1                    = ScopeBuilder.make().push() // add the root scope

    val s2 = v1.foldAST(s1, ast0)

    val meta = s2.result

    Right(State(ast0, meta))


object ScopeBuildVisitorSpec:

  final case class State(ast: AST, meta: Meta)
