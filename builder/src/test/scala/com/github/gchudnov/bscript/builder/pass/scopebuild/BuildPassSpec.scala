package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.builder.pass.Examples
import com.github.gchudnov.bscript.builder.pass.scopebuild.InState
import com.github.gchudnov.bscript.builder.pass.scopebuild.OutState
import com.github.gchudnov.bscript.builder.pass.scopebuild.PassImpl
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.const.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.util.Transform

import scala.util.control.Exception.*

/**
 * BuildPassSpec
 */
final class BuildPassSpec extends TestSpec:

  "BuildPass" when {

    "const literals" should {
      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "build scope for an integer" in {
        val t = Examples.ex21

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "var is defined" should {

      /**
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "put it in a scope" in {
        val t = Examples.ex1

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("x").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex2

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 3 // root + main(args) + block inside
            outState.symbolsByName("x").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex3

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 3 // root + main(args) + block inside
            outState.symbolsByName("x").size mustBe (2)

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex4

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 5
            outState.symbolsByName("offsetDateTime").size mustBe (1)
            outState.symbolsByName("fieldOfDateTime").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   int +(lhs: int, rhs: int) {
       *     // ...
       *   }
       *
       *   2 + 3
       * }}}
       */
      "be invoked for +(int, int): int" in {
        val t = Examples.ex5

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("+").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   double +(lhs: double, rhs: int) {
       *     // ...
       *   }
       *
       *   1.0 + 3
       * }}}
       */
      "be invoked for +(double, int): double" in {
        val t = Examples.ex6

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("+").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   int +(lhs: int, rhs: int) {
       *     // ...
       *   }
       *
       *   "abc" + 3  // incorrect, but it is reported later during compilation on the next phases
       * }}}
       */
      "be invoked for `+(int, int): int` with an invalid argument" in {
        val t = Examples.ex7

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("+").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   T +[T](lhs: T, rhs: T) {
       *     // ...
       *   }
       *
       *   4 + 3
       * }}}
       */
      "be invoked for +(T, T): T" in {
        val t = Examples.ex8

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("+").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   R +[R, T, U](lhs: T, rhs: U) {
       *     // ...
       *   }
       *
       *   4 + 3
       * }}}
       */
      "be invoked for +(T, U): R" in {
        val t = Examples.ex9

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("+").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
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
      //       case Right(BuildVisitorStateoutState) =>
      //         outState.astScopes.size mustBe (7)
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
      //       case Right(BuildVisitorStateoutState) =>
      //         outState.astScopes.size mustBe (7)
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
      //       case Right(BuildVisitorStateoutState) =>
      //         outState.astScopes.size mustBe (17)
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
      //       case Right(BuildVisitorStateoutState) =>
      //         outState.astScopes.size mustBe (15)
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
        val t = Examples.ex10

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("i").size mustBe (1)
            outState.symbolsByName("j").size mustBe (1)
            outState.symbolsByName("k").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     auto a = [1, 2, 3];
       *   }
       * }}}
       */
      "auto-defined for collections" in {
        val t = Examples.ex11

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("a").size mustBe (1)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int[] a = [1, 2, 3];
       *   }
       * }}}
       */
      "explicitly defined for collections" in {
        val t = Examples.ex12

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("a").size mustBe (1)
          case Left(t) => 
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = nothing;
       *   }
       * }}}
       */
      "allow nothing in declaration with auto-type deduction" in {
        val t = Examples.ex13

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("x").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = nothing;
       *   }
       * }}}
       */
      "allow nothing in declaration with explicit type" in {
        val t = Examples.ex14

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopeSize mustBe 1
            outState.symbolsByName("x").size mustBe (1)

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex15

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            val blockStatements = outState.ast.asInstanceOf[Block].exprs
            val callExpr        = blockStatements.last.asInstanceOf[Call]
            val callScope       = outState.scopeByAST(callExpr)

            callScope.map(_.name) mustBe Some("a")

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex16

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("x").size mustBe (1)
            outState.symbolsByName("y").size mustBe (1)
            outState.symbolsByName("j").size mustBe (1)

            // NOTE: `i` exists in 2 scopes
            outState.symbolsByName("i").size mustBe (2)

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex17

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.scopesBySymbol(SymbolRef("b")).map(_.name) must contain theSameElementsAs (List("a.c"))
            outState.scopesBySymbol(SymbolRef("c")).map(_.name) must contain theSameElementsAs (List("a.c"))
            outState.scopesBySymbol(SymbolRef("x")).map(_.name) must contain theSameElementsAs (List("a.c"))

            outState.scopesBySymbol(SymbolRef("y")).map(_.name) must contain theSameElementsAs (List("a.a"))
            outState.scopesBySymbol(SymbolRef("z")).map(_.name) must contain theSameElementsAs (List("a.b"))
            outState.scopesBySymbol(SymbolRef("i")).map(_.name) must contain theSameElementsAs (List("a.d.a.a"))

            outState.scopesBySymbol(SymbolRef("A")).map(_.name) must contain theSameElementsAs (List("a"))
            outState.scopesBySymbol(SymbolRef("B")).map(_.name) must contain theSameElementsAs (List("a"))
            outState.scopesBySymbol(SymbolRef("D")).map(_.name) must contain theSameElementsAs (List("a.d.a"))
            outState.scopesBySymbol(SymbolRef("a")).map(_.name) must contain theSameElementsAs (List("a"))
            outState.scopesBySymbol(SymbolRef("f")).map(_.name) must contain theSameElementsAs (List("a"))

          case Left(t) =>
            fail("Should be 'right", t)
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
        val t = Examples.ex18

        // TODO: failing

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("a").size mustBe (1)
          case Left(t) => 
            println(t)
            fail("Should be 'right", t)
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
        val t = Examples.ex19

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            outState.symbolsByName("f").size mustBe (1)
            outState.symbolsByName("g").size mustBe (1)
            outState.symbolsByName("main").size mustBe (1)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "a complex program" should {
      /**
       * {{{
       *   // given a collection of words, create a map of word lengths
       *   def main(): map[str, i32] = {
       *     auto as = ["alice", "bob", "carol"];
       *   
       *     val f = [](as: []str) -> map[str, i32] {
       *       val m = map[string, int]{};
       * 
       *       val iterate = [](i: i32) -> map[str, i32] {
       *         if len(m) == i {
       *           m;
       *         } else {
       *           w = get(as, i)
       *           m = set(m, w, len(w))
       *           iterate(i + 1)
       *         }
       *       }    
       *   
       *       iterate(0);
       *     };
       *   
       *     f(as); // returns a map of lengths
       *   }
       *   
       *   main();
       * }}}
       */
      "build scopes" in {
        val t = Examples.ex20

        // TODO: note that the example is not implemented yet

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(outState) =>
            ()
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
  private def eval(ast0: AST): Either[Throwable, OutState] =
    val buildPass = new PassImpl()

    for
      buildStateIn      <- nonFatalCatch.either(InState.from(ast0))
      buildOutState     <- nonFatalCatch.either(buildPass.run(buildStateIn))
    yield buildOutState
