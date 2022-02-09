package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.builder.internal.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.internal.MetaOps
import com.github.gchudnov.bscript.builder.BGlobals
import com.github.gchudnov.bscript.lang.ast.CompiledExpr
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.Show.ShowOps
import com.github.gchudnov.bscript.lang.util.{ EqWrap, Transform }
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.builder.TestSpec

/**
 * ScopeBuildVisitor tests
 */
final class ScopeResolveVisitorSpec extends TestSpec:
  import ScopeResolveVisitorSpec.*
  import Meta.*
  import MetaOps.*

  private val typeNames: TypeNames = BGlobals.typeNames

  "ScopeResolveVisitor" when {

    "var is defined" should {

      /**
       * {{{
       *   // globals
       *   int x;
       * }}}
       */
      "eval to build ast" in {
        val t = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#global") mustBe (Right(typeNames.i32Type))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "auto-var is defined" should {

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = 10; // int
       *   }
       * }}}
       */
      "resolve it" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.autoType))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "a function is defined" should {

      /**
       * {{{
       *   // globals
       *   int main() {
       *     int x;
       *     x = 3;
       *   }
       * }}}
       */
      "resolve variables" in {
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.i32Type))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "type-declarations" should {

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = 10;         // int
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("y", "#a") mustBe (Right("type:UNDEFINED")) // NOTE: it will be resolved later, in #3

          case Left(t) => fail("Should be 'right", t)
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("y", "#a") mustBe (Right("type:UNDEFINED")) // NOTE: it will be resolved later, in #3

          case Left(t) => fail("Should be 'right", t)
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.f32Type))
            typeNameForVarInScope(meta)("z", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("p", "#a") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("precision", "round") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("value", "round") mustBe (Right(typeNames.autoType))
          case Left(t) => fail("Should be 'right", t)
      }
    }

    "variables are resolved" should {

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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.i32Type))
          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = 0;
       *     x = nothing;
       *   }
       * }}}
       */
      "allow nothing in assignment" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
          Assign(Var(SymbolRef("x")), NothingVal())
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.i32Type))
          case Left(t) => fail("Should be 'right", t)
      }

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
      "sum var with other var" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(9)),
          VarDecl(TypeRef(typeNames.f32Type), "j", FloatVal(0.0f)),
          VarDecl(TypeRef(typeNames.i32Type), "k", Add(Var(SymbolRef("i")), IntVal(2)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("i", "#a") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("j", "#a") mustBe (Right(typeNames.f32Type))
            typeNameForVarInScope(meta)("k", "#a") mustBe (Right(typeNames.i32Type))
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
      "resolve collections" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(IntVal(1), IntVal(2), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("a", "#a") mustBe (Right("[]int"))
          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int[][] as = [ int[](1), int[](2), int[](3) ];
       *   }
       * }}}
       */
      "resolve collections of collections" in {
        val t = Block(
          VarDecl(
            VectorType(VectorType(TypeRef(typeNames.i32Type))),
            "as",
            Vec(
              Seq(
                Vec(Seq(IntVal(1))),
                Vec(Seq(IntVal(2))),
                Vec(Seq(IntVal(3)))
              )
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("as", "#a") mustBe (Right("[][]int"))
          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * Here:
       * {{{
       *   // globals
       *   {
       *     auto x = true;
       *     auto y = 4L;
       *     auto s = "alice"
       *     ((y + 4 >= 3) & x) or (x == "alice")
       *   }
       *
       * 1) AutoVarDecl("x", BoolVal(true)) <-- should define a variable symbol 'x'
       * 2) Var(SymbolRef("x")) <-- should return reference to the same symbol 'x', not a different one
       * }}}
       */
      "the same symbol should resolve to the same ref" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", BoolVal(true)),
          VarDecl(TypeRef(typeNames.autoType), "y", LongVal(4L)),
          VarDecl(TypeRef(typeNames.autoType), "s", StrVal("alice")),
          Or(
            And(GreaterEqual(Add(Var(SymbolRef("y")), IntVal(4)), IntVal(3)), Var(SymbolRef("x"))),
            Equal(Var(SymbolRef("x")), StrVal("alice"))
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("s", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("y", "#a") mustBe (Right(typeNames.autoType))

            val blockStatements = ast.asInstanceOf[Block].statements
            val xSymInAutoDecl  = blockStatements.head.asInstanceOf[VarDecl].symbol
            val orExpr          = blockStatements.last.asInstanceOf[Or]
            val xSymInOrLhs     = orExpr.lhs.asInstanceOf[And].rhs.asInstanceOf[Var].symbol
            val xSymInOrRhs     = orExpr.rhs.asInstanceOf[Equal].lhs.asInstanceOf[Var].symbol

            xSymInAutoDecl mustBe xSymInOrLhs
            xSymInAutoDecl mustBe xSymInOrRhs

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "scopes are defined" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int x;        // define variable x in global scope
       *     void f() {    // define function f in global scope
       *       int y;      // define variable y in local scope of f
       *       { int i; }  // define variable i in nested local scope
       *       { int j; }  // define variable j in another nested local scope
       *     }
       *
       *     void g() {    // define function g in global scope
       *       int i;      // define variable i in local scope of g
       *     }
       *   }
       * }}}
       */
      "retain scope information in AST" in {
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("i", "#e") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("j", "#d") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("y", "#b") mustBe (Right(typeNames.i32Type))
          case Left(t) => fail("Should be 'right", t)
      }
    }

    "a symbol is missing" should {

      /**
       * {{{
       *   // globals
       *   {
       *     float f(int x) {
       *         float i;
       *         { float z = x + y; i = z; }
       *     }
       *   }
       * }}}
       */
      "report an error" in {
        val t = MethodDecl(
          TypeRef(typeNames.f32Type),
          "f",
          List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
          Block(
            VarDecl(TypeRef(typeNames.f32Type), "i", FloatVal(0.0f)),
            Block(
              VarDecl(TypeRef(typeNames.f32Type), "z", Add(Var(SymbolRef("x")), Var(SymbolRef("y")))), // NOTE: `y` declaration is missing
              Assign(Var(SymbolRef("i")), Var(SymbolRef("z")))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage.contains("Symbol 'y'") mustBe (true)
      }
    }

    "nested scopes" should {

      /**
       * {{{
       *   // globals
       *   {
       *     { int z; }       // local scope nested within f's local scope
       *     printf("%d", z); // z is no longer visible; static analysis ERROR!
       *   }
       * }}}
       */
      "report an error if Var in Call is not visible" in {
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
          Call(SymbolRef("printf"), List(StrVal("%d"), Var(SymbolRef("z")))) // z is no longer visible; ERROR!
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage.contains("Symbol 'z'") mustBe (true)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x;             // define variable x in global scope
       *     int y;             // define variable y in global space
       *     void f() {         // define function f in global scope
       *       float x;         // redefine x as local variable, hiding outer x
       *       printf("%f", x); // x resolves to f's local
       *       printf("%d", y); // y resolves to global
       *       { int z; }       // local scope nested within f's local scope
       *       printf("%d", z); // z is no longer visible; static analysis ERROR!
       *     }
       *   }
       * }}}
       */
      "report an error if Var in Call is not visible with several nested scopes" in {
        val t = BGlobals.prelude ++ Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
          VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(0)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef(typeNames.f32Type), "x", FloatVal(0.0f)),
              Call(SymbolRef("printf"), List(StrVal("%f"), Var(SymbolRef("x")))),
              Call(SymbolRef("printf"), List(StrVal("%d"), Var(SymbolRef("y")))),
              Block(
                VarDecl(TypeRef(typeNames.i32Type), "z", IntVal(0))
              ),
              Call(SymbolRef("printf"), List(StrVal("%d"), Var(SymbolRef("z")))) // z is no longer visible; ERROR!
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage.contains("Symbol 'z'") mustBe (true)
      }
    }

    "all symbols can be resolved" should {

      /**
       * {{{
       * // globals
       * {
       *   int i = 9;
       *   float f(int x, float y) {
       *     float i;
       *     { float z = x + y; i = z; }
       *     { float z = i + 1; i = z; }
       *     return i;
       *   }
       *
       *   void g() {
       *     f(i, 2);
       *   }
       * }
       * }}}
       */
      "create all scopes" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(9)),
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              VarDecl(TypeRef(typeNames.f32Type), "i", FloatVal(0.0f)),
              Block(
                VarDecl(TypeRef(typeNames.f32Type), "z", Add(Var(SymbolRef("x")), Var(SymbolRef("y")))),
                Assign(Var(SymbolRef("i")), Var(SymbolRef("z")))
              ),
              Block(
                VarDecl(TypeRef(typeNames.f32Type), "z", Add(Var(SymbolRef("i")), IntVal(1))),
                Assign(Var(SymbolRef("i")), Var(SymbolRef("z")))
              ),
              Var(SymbolRef("i"))
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List.empty[ArgDecl],
            Block(
              Call(SymbolRef("f"), List(Var(SymbolRef("i")), IntVal(2)))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "f") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("y", "f") mustBe (Right(typeNames.f32Type))
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
      "resolve struct fields" in {
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("a", "#a") mustBe (Right("A"))
            typeNameForVarInScope(meta)("b", "A") mustBe (Right("B"))
            typeNameForVarInScope(meta)("c", "A") mustBe (Right("C"))
            typeNameForVarInScope(meta)("d", "#b") mustBe (Right("D"))
            typeNameForVarInScope(meta)("i", "D") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("x", "A") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("y", "B") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("z", "C") mustBe (Right(typeNames.i32Type))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct A {
       *       int x;
       *       float y;
       *     };
       *
       *     void f() {
       *       A a;      // define a new A struct called a
       *       a.x = 1;
       *     }
       *   }
       * }}}
       */
      "can reference fields in data aggregates" in {
        val t = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.f32Type), "y"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
              Assign(Access(Var(SymbolRef("a")), Var(SymbolRef("x"))), IntVal(1))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("a", "#b") mustBe (Right("A"))
            typeNameForVarInScope(meta)("x", "A") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("y", "A") mustBe (Right(typeNames.f32Type))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct A {
       *       int x;
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
       *       d.a = 10; // NOTE: here there is no 'a' field in D-structure
       *     }
       *   }
       * }}}
       */
      "report an error if a struct field is not in the struct" in {
        val t = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              StructDecl("D", List(FieldDecl(TypeRef(typeNames.i32Type), "i"))),
              VarDecl(TypeRef("D"), "d", Init(TypeRef("D"))),
              Assign(Access(Var(SymbolRef("d")), Var(SymbolRef("a"))), IntVal(1))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Symbol 'd.a' cannot be resolved")
      }
    }

    "auto-declarations" should {

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = true;     // bool
       *     auto y = 3;        // long
       *     auto s = "alice";  // string
       *   }
       * }}}
       */
      "be correctly processed" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", BoolVal(true)),
          VarDecl(TypeRef(typeNames.autoType), "y", LongVal(4L)),
          VarDecl(TypeRef(typeNames.autoType), "s", StrVal("alice"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("s", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.autoType))
            typeNameForVarInScope(meta)("y", "#a") mustBe (Right(typeNames.autoType))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "method-declarations" should {

      /**
       * {{{
       *   // globals
       *   // printf // definition only with compiled expr
       * }}}
       */
      "printf" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "printf",
            List(
              ArgDecl(TypeRef(typeNames.strType), "format"),
              ArgDecl(TypeRef(typeNames.autoType), "value")
            ),
            Block.empty
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("format", "printf") mustBe (Right(typeNames.strType))
            typeNameForVarInScope(meta)("value", "printf") mustBe (Right(typeNames.autoType))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   // offsetDateTime // definition only with compiled expr
       * }}}
       */
      "offsetDateTime" in {
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
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("offset", "offsetDateTime") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("unit", "offsetDateTime") mustBe (Right(typeNames.strType))
            typeNameForVarInScope(meta)("value", "offsetDateTime") mustBe (Right(typeNames.datetimeType))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   // offsetDateTime   // definition
       *   // fieldOfDateTime  // definition
       * }}}
       *
       * NOTE: we need this test, since an error was triggered when two ArgDecl(x, y) were in different functions.
       */
      "offsetDateTime & fieldOfDateTime" in {
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

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            val units          = meta.varTypes.filter(_._1.value.name == "unit").keys.toList
            val (unit1, unit2) = (units.head, units.last)
            units.size mustBe (2)
            unit1 mustNot be(unit2) // unit1 & unit2 are different references

            val values           = meta.varTypes.filter(_._1.value.name == "value").keys.toList
            val (value1, value2) = (values.head, values.last)
            values.size mustBe (2)
            value1 mustNot be(value2) // value1 & value2 are different references

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   // prelude
       *   {
       *     void f() {         // define function f in global scope
       *       float x;         // redefine x as local variable, hiding outer x
       *       printf("%f", x); // x resolves to f's local
       *       printf("%d", y); // y resolves to global
       *       { int z; }       // local scope nested within f's local scope
       *       printf("%d", z); // z is no longer visible; static analysis ERROR!
       *     }
       *   }
       * }}}
       *
       * NOTE: we need this test to make sure that everything in prelude resolves.
       */
      "be resolved with compiled statements" in {
        val t = BGlobals.prelude ++ Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef(typeNames.f32Type), "x", FloatVal(0.0f))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(ScopeResolveVisitorState(ast, meta)) => succeed
          case Left(t)                                    => fail("Should be 'right", t)
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
      "resolve symbols" in {
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
          case Right(ScopeResolveVisitorState(ast, meta)) =>
            findMethodAst(meta, "f").isDefined mustBe (true)
            findMethodAst(meta, "g").isDefined mustBe (true)
            findMethodAst(meta, "main").isDefined mustBe (true)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phase 1 and 2
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve symbols that were populated in Phase-1
   */
  private def eval(ast0: AST): Either[Throwable, ScopeResolveVisitorState] =
    val (initMeta, rootScope) = BGlobals.make()
    val v1                    = ScopeBuildVisitor.make()
    val s1                    = ScopeBuildState.make(ast0, initMeta, rootScope, Gen.empty)

    ast0
      .visit(s1, v1)
      .flatMap { s11 =>
        val v2   = ScopeResolveVisitor.make()
        val s2   = ScopeResolveState.make(s11.ast, s11.meta)
        val ast1 = s11.ast

        ast1
          .visit(s2, v2)
          .flatMap { s21 =>
            val t    = ScopeResolveVisitorState(meta = s21.meta, ast = s21.ast)
            val ast2 = s21.ast

            // println(t.meta.show())

            // { AST->Scope } size must be the same before and after Phase #2
            s11.meta.astScopes.size mustEqual (s21.meta.astScopes.size)

            ast2
              .visit(".", verifyResolved(t))
              .map(_ => t)
          }
      }

object ScopeResolveVisitorSpec:
  import MetaOps.*

  final case class ScopeResolveVisitorState(ast: AST, meta: Meta)

  def verifyResolved(t: ScopeResolveVisitorState): TreeVisitor[String, Unit] = new TreeVisitor[String, Unit]:

    override def visit(s: String, n: Init): Either[Throwable, Unit] = for _ <- checkType(n.iType, s"${s} -> Init(iType=${n.iType.name}")
    yield ()

    override def visit(s: String, n: UnaryMinus): Either[Throwable, Unit] = for _ <- n.expr.visit(s"${s} -> UnaryMinus(expr", this)
    yield ()

    override def visit(s: String, n: Add): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Add(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Add(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Sub): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Sub(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Sub(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Mul): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Mul(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mul(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Div): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Div(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Div(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Mod): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Mod(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mod(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Less): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Less(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Less(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: LessEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> LessEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> LessEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Greater): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Greater(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Greater(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: GreaterEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> GreaterEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> GreaterEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Equal): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Equal(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Equal(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: NotEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> NotEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> NotEqual(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Not): Either[Throwable, Unit] = for _ <- n.expr.visit(s"${s} -> Not(expr", this)
    yield ()

    override def visit(s: String, n: And): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> And(lhs", this)
      _ <- n.rhs.visit(s"${s} -> And(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Or): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Or(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Or(lhs, rhs", this)
    yield ()

    override def visit(s: String, n: Assign): Either[Throwable, Unit] = for
      _ <- n.id.visit(s"${s} -> Assign(id", this)
      _ <- n.expr.visit(s"${s} -> Assign(id, expr", this)
    yield ()

    override def visit(s: String, n: NothingVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: VoidVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: BoolVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: IntVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: LongVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: FloatVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: DoubleVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: DecimalVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: StrVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: DateVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: DateTimeVal): Either[Throwable, Unit] = Right(())

    override def visit(s: String, n: Vec): Either[Throwable, Unit] =
      Transform.sequence(n.elements.zipWithIndex.map { case (expr, i) => expr.visit(s"${s} -> Vec(${i})", this) }).map(_ => ())

    override def visit(s: String, n: Var): Either[Throwable, Unit] = for _ <- checkSymbol(n.symbol, s"${s} -> Var(symbol=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: ArgDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.aType, s"${s} -> ArgDecl(aType=${n.aType.name}")
      _ <- checkSymbol(n.symbol, s"${s} -> ArgDecl(aType=${n.aType.name},name=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: VarDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.vType, s"${s} -> VarDecl(vType=${n.vType.name}")
      _ <- n.expr.visit(s"${s} -> VarDecl(vType=${n.vType.name},name=${n.name},expr", this)
      _ <- checkSymbol(n.symbol, s"${s} -> VarDecl(symbol=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: FieldDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.fType, s"${s} -> FieldDecl(fType=${n.fType.name}")
      _ <- checkSymbol(n.symbol, s"${s} -> FieldDecl(symbol=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: MethodDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.retType, s"${s} -> MethodDecl(retType=${n.retType.name}")
      _ <- Transform.sequence(n.params.map(p => p.visit(s"${s} -> MethodDecl(name=${n.name},param=${p.name}", this)))
      _ <- n.body.visit(s"${s} -> MethodDecl(name=${n.name},body", this)
      _ <- checkSymbol(n.symbol, s"${s} -> MethodDecl(symbol=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: StructDecl): Either[Throwable, Unit] = for
      _ <- Transform.sequence(n.fields.map(p => p.visit(s"${s} -> StructDecl(name=${n.name},field=${p.name}", this)))
      _ <- checkSymbol(n.symbol, s"${s} -> StructDecl(symbol=${n.symbol.name}")
    yield ()

    override def visit(s: String, n: Block): Either[Throwable, Unit] =
      Transform.sequence(n.statements.map(st => st.visit(s"${s} -> Block(stmt=${st}", this))).map(_ => ())

    override def visit(s: String, n: Call): Either[Throwable, Unit] = for
      _ <- checkSymbol(n.id, s"${s} -> Call(id=${n.id.name}")
      _ <- Transform.sequence(n.args.map(e => e.visit(s"${s} -> Call(id=${n.id.name},arg=${e}", this)))
    yield ()

    override def visit(s: String, n: If): Either[Throwable, Unit] = for
      _ <- n.cond.visit(s"${s} -> If(cond=${n.cond}", this)
      _ <- n.then1.visit(s"${s} -> If(then1=${n.then1}", this)
      _ <- Transform.sequence(n.else1.map(_.visit(s"${s} -> If(else=${n.else1}", this)))
    yield ()

    override def visit(s: String, n: Access): Either[Throwable, Unit] = for
      _ <- n.a.visit(s"${s} -> Access(a=${n.a}", this)
      _ <- n.b.visit(s"${s} -> Access(b=${n.b}", this)
    yield ()

    override def visit(s: String, n: CompiledExpr): Either[Throwable, Unit] = Right(())

    private def checkType(t: Type, msg: String, isCheckedAsSymbol: Boolean = false): Either[Throwable, Unit] =
      for
        _ <- Either.cond(!t.isInstanceOf[TypeRef], (), new AstException(s"[Type] '${t.name}' must be a Type, not a TypeRef: [${msg}]."))
        _ <- Either.cond(!t.equals(Type.Undefined), (), new AstException(s"[Type] '${t.name}' must be defined, but got '${Type.Undefined}': [${msg}]."))
        _ <- t match
               case DeclType(expr: Expr) =>
                 expr.visit(s"${msg} -> DeclType(expr=", this)
               case _ => Right(())
        _ <- t match
               case s: Symbol if (!isCheckedAsSymbol) => checkSymbol(s, msg, isCheckedAsType = true)
               case _                                 => Right(())
      yield ()

    private def checkSymbol(s: Symbol, msg: String, isCheckedAsType: Boolean = false): Either[Throwable, Unit] =
      for
        _ <- Either.cond(!s.isInstanceOf[SymbolRef], (), new AstException(s"[Symbol] '${s.name}' SymbolRef must be replaced by a Symbol, but it was not: [${msg}]."))
        _ <- Either.cond(!s.equals(Symbol.Undefined), (), new AstException(s"[Symbol] '${s.name}' Symbol must be defined, but it was not [${msg}]."))
        _ <- Either.cond(findSymbolScope(t.meta, s).nonEmpty, (), new AstException(s"[Symbol] '${s.name}' Symbol must have a defined Scope [${msg}]."))
        _ <- Either.cond(
               findSymbolScope(t.meta, s).flatMap(scope => findMember(t.meta, s.name, scope)).isDefined,
               (),
               new AstException(s"[Symbol] '${s.name}' Symbol's scope must contain the Symbol, but it didn't: [${msg}].")
             )
        _ <- s match
               case vs: SVar => checkSVar(vs, msg)
               case _        => Right(())
        _ <- s match
               case ms: SMethod => checkSMethod(ms, msg)
               case _           => Right(())
        _ <- s match
               case ss: SStruct => checkSStruct(ss, msg)
               case _           => Right(())
        _ <- s match
               case t: Type if (!isCheckedAsType) => checkType(t, msg, isCheckedAsSymbol = true)
               case _                             => Right(())
      yield ()

    private def checkSVar(vs: SVar, msg: String): Either[Throwable, Unit] =
      for _ <- Either.cond(findType(t.meta, vs).isDefined, (), new AstException(s"[SVar] '${vs.name}' Type for SVar is not found: [${msg}]."))
      yield ()

    private def checkSMethod(ms: SMethod, msg: String): Either[Throwable, Unit] =
      for
        _ <- Either.cond(findRetType(t.meta, ms).isDefined, (), new AstException(s"[SMethod] '${ms.name}' Type for SMethod is not found: [${msg}]."))
        _ <- Transform.sequence(t.meta.methodArgs.getOrElse(EqWrap(ms), List.empty[SVar]).map(s => checkSymbol(s, s"[${msg}] -> SMethod(arg=${s}")))
      yield ()

    private def checkSStruct(ss: SStruct, msg: String): Either[Throwable, Unit] =
      for _ <- Either.cond(
                 t.meta.symbolScopes.contains(EqWrap(ss.asInstanceOf[Symbol])),
                 (),
                 new AstException(s"[SStruct] Scope for SStruct '${ss.name}' is not found: [${msg}].")
               )
      yield ()
