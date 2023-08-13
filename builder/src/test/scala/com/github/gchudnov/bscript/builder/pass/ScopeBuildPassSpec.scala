package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

import scala.util.control.Exception.*
import com.github.gchudnov.bscript.builder.Examples

/**
 * Scope Build Pass Tests
 */
final class ScopeBuildPassSpec extends TestSpec:
  import ScopeBuildPassSpec.*

  "ScopeBuildPass" when {

    "const literals" should {

      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "build scope for an integer" in {
        val t = Examples.intVal

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val actualScopeTree = actualState.scopeTree.asString
            val expectedScopeTree = """|{
                                       |  "vertices": ["scope(0)"],
                                       |  "edges": []
                                       |}
                                       |""".stripMargin

            actualState.scopeSymbols.size mustBe 1
            actualState.scopeAsts.size mustBe 1

            actualScopeTree mustBe expectedScopeTree

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
        val t = Examples.varDef

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val actualScopeSymbols = actualState.scopeSymbols.asString

            val actualScopeTree = actualState.scopeTree.asString
            val expectedScopeTree = """|{
                                       |  "vertices": ["scope(0)"],
                                       |  "edges": []
                                       |}
                                       |""".stripMargin

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("x"))

            actualState.scopeSymbols.size mustBe 1
            actualState.scopeAsts.size mustBe 1

            actualScopeSymbols must include("symbol(SVar(x))")

            actualScopeTree mustBe expectedScopeTree
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   int x = 1;
       * }}}
       */
      "raise an error if a var is declared several times in a scope" in {
        val t = Examples.doubleDef

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("already defined")
      }

    }

    "functions" should {

      /**
       * {{{
       *   // globals
       *   fn main() -> int = {
       *     0;
       *   }
       *
       *   fn main() -> int = {
       *     1;
       *   }
       * }}}
       */
      "raise an error if a method is declared several times with the same signature in a scope" in {
        val t = Examples.defMethodSameSig

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("already defined")
      }

      /**
       * {{{
       *   // globals
       *   fn main(x: int) -> int = {
       *     0;
       *   }
       *
       *   fn main() -> int = {
       *     1;
       *   }
       * }}}
       */
      "no error if there are several methods with the same name, but a different signatures" in {
        val t = Examples.defMethodDiffSig

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("main", "<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("main", "<>(i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("x"))

          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   fn main() -> long = {
       *     0L;
       *   }
       *
       *   fn main() -> int = {
       *     1;
       *   }
       * }}}
       */
      "raies an error if there are two methods with the same name, but the signature is different only in the return type" in {
        val t = Examples.defMethodDiffRetType

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("already defined")
      }

      /**
       * {{{
       *   fn +[R, T, U](lhs: T, rhs: U) -> R {
       *     // ...
       *   }
       *
       *   fn +[X, Y, Z](lhs: Y, rhs: Z) -> X {
       *     // ...
       *   }
       * }}}
       */
      "raise an error if there are two generic methods that different only in naming" in {
        val t = Examples.twoSameGenericMethods

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("already defined")
      }

      /**
       * {{{
       *   // globals
       *   fn main() -> int = {
       *     int x;
       *     x = 3;
       *   }
       * }}}
       */
      "define nested scopes" in {
        val t = Examples.fnDecl

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            val actualScopeTree = actualState.scopeTree.asString
            val expectedScopeTree = """|{
                                       |  "vertices": ["scope(0)","scope(0.0)","scope(0.0.0)"],
                                       |  "edges": [["scope(0.0)","scope(0)"],["scope(0.0.0)","scope(0.0)"]]
                                       |}
                                       |""".stripMargin

            actualState.scopeSize mustBe 3 // root + main(args) + block inside

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("main", "<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0.0")) must contain(SVar("x"))

            actualScopeTree mustBe expectedScopeTree

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
        val t = Examples.fnVarArgShadow

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            val actualScopeTree = actualState.scopeTree.asString
            val expectedScopeTree = """|{
                                       |  "vertices": ["scope(0)","scope(0.0)","scope(0.0.0)"],
                                       |  "edges": [["scope(0.0)","scope(0)"],["scope(0.0.0)","scope(0.0)"]]
                                       |}
                                       |""".stripMargin

            actualState.scopeSize mustBe 3 // root + main(args) + block inside

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("myFunc", "<>(i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("x"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0.0")) must contain(SVar("x"))

            actualScopeTree mustBe expectedScopeTree

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
        val t = Examples.twoMethods

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            val actualScopeTree = actualState.scopeTree.asString
            val expectedScopeTree = """|{
                                       |  "vertices": ["scope(0)","scope(0.0)","scope(0.0.0)","scope(0.1)","scope(0.1.1)"],
                                       |  "edges": [["scope(0.0)","scope(0)"],["scope(0.0.0)","scope(0.0)"],["scope(0.1)","scope(0)"],["scope(0.1.1)","scope(0.1)"]]
                                       |}
                                       |""".stripMargin

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("fieldOfDateTime", "<>(datetime, str)"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("offsetDateTime", "<>(datetime, i32, str)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("offset"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("unit"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("value"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("unit"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("value"))

            actualScopeTree mustBe expectedScopeTree

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
        val t = Examples.plusInt

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("+", "<>(i32, i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("lhs"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("rhs"))

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
        val t = Examples.plusDoubleInt

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("+", "<>(f64, i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("lhs"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("rhs"))

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
        val t = Examples.plusStringInt

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("+", "<>(i32, i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("lhs"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("rhs"))

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
        val t = Examples.plusGeneric

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("+", "<A>(A, A)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SType("T"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("lhs"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("rhs"))

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
        val t = Examples.plusT

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("+", "<A, B, C>(B, C)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SType("R"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SType("T"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SType("U"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("lhs"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("rhs"))

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
      //       case Right(BuildVisitorStateactualState) =>
      //         actualState.astScopes.size mustBe (7)
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
      //       case Right(BuildVisitorStateactualState) =>
      //         actualState.astScopes.size mustBe (7)
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
      //       case Right(BuildVisitorStateactualState) =>
      //         actualState.astScopes.size mustBe (17)
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
      //       case Right(BuildVisitorStateactualState) =>
      //         actualState.astScopes.size mustBe (15)
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
        val t = Examples.multipleVars

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("i"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("j"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("k"))

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
        val t = Examples.autoCol

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("a"))

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
        val t = Examples.varCol

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("a"))

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
        val t = Examples.autoNothing

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("x"))

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
        val t = Examples.intNothing

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("x"))

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
        val t = Examples.varNotInScope

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("printf", "<>(str, auto)"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("format"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("value"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("z"))

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
        val t = Examples.nestedScopes

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("f", "<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("g", "<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("x"))

            actualState.scopeSymbols.symbols(ScopeRef("0.0.0")) must contain(SVar("y"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0.0.0")) must contain(SVar("i"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0.0.1")) must contain(SVar("j"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1.1")) must contain(SVar("i"))

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
        val t = Examples.struct

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("f", "<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SStruct("A"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SStruct("B"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SStruct("C"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("a"))

            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("y"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("z"))
            actualState.scopeSymbols.symbols(ScopeRef("0.2")) must contain(SVar("b"))
            actualState.scopeSymbols.symbols(ScopeRef("0.2")) must contain(SVar("c"))
            actualState.scopeSymbols.symbols(ScopeRef("0.2")) must contain(SVar("x"))

            actualState.scopeSymbols.symbols(ScopeRef("0.3.0")) must contain(SStruct("D"))
            actualState.scopeSymbols.symbols(ScopeRef("0.3.0")) must contain(SVar("d"))
            actualState.scopeSymbols.symbols(ScopeRef("0.3.0.0")) must contain(SVar("i"))

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
        val t = Examples.structInit

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SStruct("A"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SStruct("B"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("a"))

            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("y"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("b"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("s"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("x"))

          case Left(t) =>
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
        val t = Examples.smallApp

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>

            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("f","<>(i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("g","<>(i32)"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SMethod("main","<>()"))
            actualState.scopeSymbols.symbols(ScopeRef("0")) must contain(SVar("x"))

            actualState.scopeSymbols.symbols(ScopeRef("0.0")) must contain(SVar("x"))
            actualState.scopeSymbols.symbols(ScopeRef("0.0.0")) must contain(SVar("z"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1")) must contain(SVar("x"))
            actualState.scopeSymbols.symbols(ScopeRef("0.1.1")) must contain(SVar("y"))

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
        val t = Examples.advApp

        // TODO: note that the example is not implemented yet

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            ()
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "ScopeBuildState" should {
      val state0 = ScopeBuildState.empty

      "prohibit symbol definition if no scope was pushed" in {
        val sym = SType.bool

        assertThrows[BuilderException] {
          state0.defineSymbol(sym)
        }
      }

      "return state with one scope if a scope was pushed" in {
        val state1 = state0.pushScope()

        state1.scopeTree.vertexSize mustBe 1
        state1.scopeTree.edgeSize mustBe 0
      }

      "link a symbol to this scope if a scope was pushed" in {
        val state1 = state0.pushScope()

        val sym = SType.f32

        val state2 = state1.defineSymbol(sym)

        state2.scopeTree.vertexSize mustBe 1
        state2.scopeTree.edgeSize mustBe 0

        state2.scopeSymbols.symbols mustBe List(sym)
      }
    }
  }

  /**
   * To evaluate, we run Phase 1 only.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 build scopes
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0
    val buiOut = buildPass.run(buildIn)

    // return the actual state
    val actualState = toActualState(buiOut)
    actualState
  }

object ScopeBuildPassSpec:

  final case class ActualState(
    scopeTree: ScopeTree,
    scopeAsts: ScopeAsts,
    scopeSymbols: ScopeSymbols,
  ):
    /**
     * Get the number of scopes in the AST
     */
    def scopeSize: Int =
      scopeTree.vertexSize

    /**
     * Find Scope by AST
     */
    def scopeByAST(ast: AST): Option[Scope] =
      scopeAsts.scope(ast)

    /**
     * Find all symbols that have the given name
     */
    def symbols: List[Symbol] =
      scopeSymbols.symbols

  def toActualState(s: HasScopeTree & HasScopeSymbols & HasScopeAsts): ActualState =
    ActualState(
      scopeTree = s.scopeTree,
      scopeAsts = s.scopeAsts,
      scopeSymbols = s.scopeSymbols,
    )
