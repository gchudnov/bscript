package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.builder.internal.ScopeBuildVisitor.ScopeBuildState
import com.github.gchudnov.bscript.builder.internal.ScopeResolveVisitor.ScopeResolveState
import com.github.gchudnov.bscript.builder.internal.TypeCheckVisitor.TypeCheckState
import com.github.gchudnov.bscript.builder.BGlobals
import com.github.gchudnov.bscript.builder.BTypeCheckLaws
import com.github.gchudnov.bscript.lang.ast.CompiledExpr
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.{ TypeNames, Types }
import com.github.gchudnov.bscript.lang.util.Show.ShowOps
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.builder.util.Gen
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.builder.internal.MetaOps
import com.github.gchudnov.bscript.builder.TestSpec

final class TypeCheckVisitorSpec extends TestSpec:
  import TypeCheckVisitorSpec.*
  import Meta.*
  import MetaOps.*

  private val typeNames: TypeNames = BGlobals.typeNames
  private val types: Types         = Types.make(typeNames)

  "TypeCheckVisitor" when {

    "type-declarations" should {

      /**
       * {{{
       *   // globals
       *   {
       *     auto x = 10; // int
       *     decltype(x) y = 20;
       *     y;
       *   }
       * }}}
       */
      "can reference auto-type" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block    = ast.asInstanceOf[Block]
            val autoDecl = block.statements(0).asInstanceOf[VarDecl]
            val varDecl  = block.statements(1).asInstanceOf[VarDecl]
            val yVar     = block.statements(2).asInstanceOf[Var]

            autoDecl.evalType.name mustBe (typeNames.voidType)
            autoDecl.expr.evalType.name mustBe (typeNames.i32Type)
            varDecl.vType.name mustBe (typeNames.i32Type)
            yVar.evalType.name mustBe (typeNames.i32Type)

          case Left(t) => fail("Should be 'right", t)
      }

      "can reference auto-type in vectors" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
          VarDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs", Vec(List(IntVal(10), IntVal(20)))),
          Var(SymbolRef("xs"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            val xsVar = block.statements(2).asInstanceOf[Var] // NOTE: line 3 (last line in the block)

            xsVar.evalType.name mustBe (s"[]${typeNames.i32Type}")

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int x = 10;
       *     decltype(x) y = 20;
       *
       *     y;
       *   }
       * }}}
       */
      "can reference real type" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(10)),
          VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
          Var(SymbolRef("y"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block    = ast.asInstanceOf[Block]
            val xVarDecl = block.statements(0).asInstanceOf[VarDecl]
            val yVarDecl = block.statements(1).asInstanceOf[VarDecl]
            val yVar     = block.statements(2).asInstanceOf[Var]

            xVarDecl.evalType.name mustBe (typeNames.voidType)
            xVarDecl.expr.evalType.name mustBe (typeNames.i32Type)
            yVarDecl.vType.name mustBe (typeNames.i32Type)
            yVar.evalType.name mustBe (typeNames.i32Type)

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
      "can be present in functions return types" in {
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
          case Right(TypeCheckVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "#a") mustBe (Right(typeNames.f32Type))
            typeNameForVarInScope(meta)("p", "#a") mustBe (Right(typeNames.i32Type))
            typeNameForVarInScope(meta)("z", "#a") mustBe (Right(typeNames.f32Type))

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     fn contains(auto x, decltype(x)[] xs) -> bool { ... }
       *
       *     bool x = contains(1, []int{ 1, 2, 3 });
       *     x;
       *   }
       * }}}
       */
      "be allowed in function arguments" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.boolType),
            "contains",
            List(
              ArgDecl(TypeRef(typeNames.autoType), "x"),
              ArgDecl(VectorType(DeclType(Var(SymbolRef("x")))), "xs")
            ),
            Block(
              CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.boolType))
            ),
            Seq(ComAnn("Tests whether the list contains the given element."), StdAnn())
          ),
          VarDecl(
            TypeRef(typeNames.boolType),
            "x",
            Call(SymbolRef("contains"), List(IntVal(1), Vec(List(IntVal(1), IntVal(2), IntVal(3)))))
          ),
          Var(SymbolRef("x"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("x", "contains") mustBe (Right(typeNames.autoType))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "no type promotion is needed" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct B { int y; };
       *     struct A {
       *       int x;
       *       B b;
       *     };
       *
       *     int i = 0;
       *     int j = 0;
       *
       *     void f() {
       *       A a;
       *       a.x = (1 + i) * j;
       *       a.b.y = 2;
       *       boolean b = 3 == a.x;
       *       if ( i < j ) f();
       *     }
       *   }
       * }}}
       */
      "resolve types" in {
        val t = Block(
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0)),
          VarDecl(TypeRef(typeNames.i32Type), "j", IntVal(0)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
              Assign(
                Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
                Mul(Add(IntVal(1), Var(SymbolRef("i"))), Var(SymbolRef("j")))
              ),
              Assign(
                Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
                IntVal(2)
              ),
              VarDecl(TypeRef("boolean"), "b", Equal(IntVal(3), Access(Var(SymbolRef("a")), Var(SymbolRef("x"))))),
              If(Less(Var(SymbolRef("i")), Var(SymbolRef("j"))), Call(SymbolRef("f"), List.empty[Expr]))
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            typeNameForVarInScope(meta)("a", "#b") mustBe (Right("A"))
            typeNameForVarInScope(meta)("b", "A") mustBe (Right("B"))
            typeNameForVarInScope(meta)("b", "#b") mustBe (Right(typeNames.boolType))

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "type promotion is needed" should {

      /**
       * {{{
       *   // globals
       *   (3 + 4.2f) + 1.0;
       * }}}
       */
      "types are promoted" in {
        val t = Add(Add(IntVal(3), FloatVal(4.2f)), DoubleVal(1.0))

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            ast.asInstanceOf[Add].evalType.name mustBe (typeNames.f64Type)
            ast.asInstanceOf[Add].promoteToType mustBe (None)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     float a[];
       *     int b[];
       *     double c = 1.4f + 1; // promote to double
       *
       *     float g(int x, float y) {
       *       return 10;         // promote 10 to float
       *     }
       *
       *     void f() {
       *       g(1, 10);          // second arg promotion to float
       *     }
       *   }
       * }}}
       *
       * NOTE: here the order of 'f' and 'g' definition is not important, since we first build scopes in 'ScopeBuildVisitor' and then resolve them in 'ScopeResolveVisitor'.
       */
      "types are promoted for function calls" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.f32Type)), "a", Init(VectorType(TypeRef(typeNames.f32Type)))),
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "b", Init(VectorType(TypeRef(typeNames.i32Type)))),
          VarDecl(TypeRef(typeNames.f64Type), "c", Add(FloatVal(1.4f), IntVal(1))),
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              IntVal(10)
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block(
              Call(SymbolRef("g"), List(IntVal(1), IntVal(10))),
              VoidVal()
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]

            // promote to double
            typeNameForVarInScope(meta)("c", "#a") mustBe (Right("double"))

            // promote 10 to float
            block.statements(3).asInstanceOf[MethodDecl].body.evalType.name mustBe (typeNames.i32Type)
            block.statements(3).asInstanceOf[MethodDecl].body.promoteToType.map(_.name).get mustBe (typeNames.f32Type)

            // g(1, 10); -- second arg promotion to float
            block.statements(4).asInstanceOf[MethodDecl].body.statements(0).asInstanceOf[Call].args(1).promoteToType.map(_.name).get mustBe (typeNames.f32Type)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   "a" + 12
       * }}}
       */
      "reports an error if operation is not permitted" in {
        val t = Add(StrVal("a"), IntVal(12))

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Operation +('string', 'int') is not supported")
      }
    }

    "unary minus" should {

      /**
       * {{{
       *   // globals
       *   -true
       * }}}
       */
      "return type error for unsupported type" in {
        val t = UnaryMinus(BoolVal(true))

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Operation -('boolean') is not supported")
      }

      /**
       * {{{
       *   // globals
       *   -17
       * }}}
       */
      "be invoked on supported types" in {
        val t = UnaryMinus(IntVal(17))

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val unaryMinus = ast.asInstanceOf[UnaryMinus]
            unaryMinus.evalType.name mustBe (typeNames.i32Type)
            unaryMinus.promoteToType.map(_.name) mustBe (None)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "unary negation (not)" should {

      /**
       * {{{
       *   // globals
       *   !56.2f
       * }}}
       */
      "return an error on unsupported types" in {
        val t = Not(FloatVal(56.2f))

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Operation !('float') is not supported")
      }

      /**
       * {{{
       *   // globals
       *   !false
       * }}}
       */
      "be invoked on supported types" in {
        val t = Not(BoolVal(false))

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val notExpr = ast.asInstanceOf[Not]
            notExpr.evalType.name mustBe (typeNames.boolType)
            notExpr.promoteToType.map(_.name) mustBe (None)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "boolean expression" should {

      /**
       * {{{
       *   // globals
       *   true && false
       * }}}
       */
      "be type checked" in {
        val t = And(BoolVal(true), BoolVal(false))

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val andExpr = ast.asInstanceOf[And]
            andExpr.evalType.name mustBe (typeNames.boolType)
            andExpr.promoteToType.map(_.name) mustBe (None)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   true && 1
       * }}}
       */
      "cannot be invoked on non-booleans" in {
        val t = And(BoolVal(true), IntVal(1))

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Operation &&('boolean', 'int') is not supported")
      }
    }

    "method call" should {

      /**
       * {{{
       *   // globals
       *   {
       *     float g(int x, float y) {
       *       return 10;
       *     }
       *
       *     g("123", true);
       *   }
       * }}}
       */
      "fail if argument types are no matching and cannot be promoted" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              IntVal(10)
            )
          ),
          Call(SymbolRef("g"), List(StrVal("123"), BoolVal(true)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot convert argument 'x' type from 'string' to 'int' in 'g' method call")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     float g(int x, float y) {
       *       return 10;
       *     }
       *
       *     g(78, 12.34f);
       *   }
       * }}}
       *
       * NOTE: there is no Return-statement in AST, since everything we have are expressions.
       */
      "succeed if argument types are matching or can be promoted" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              IntVal(10)
            )
          ),
          Call(SymbolRef("g"), List(IntVal(78), FloatVal(12.34f)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            block.statements(1).asInstanceOf[Call].args(0).evalType.name mustBe (typeNames.i32Type)
            block.statements(1).asInstanceOf[Call].args(1).evalType.name mustBe (typeNames.f32Type)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "return statement" should {

      /**
       * {{{
       *   // globals
       *   {
       *     float g(int x, float y) {
       *       return "ABC";
       *     }
       *   }
       * }}}
       *
       * NOTE: there is no Return-statement in AST, since everything we have are expressions.
       */
      "raise an error if incompatible type is returned" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              StrVal("ABC")
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot convert type 'string' to 'float' in the return statement of method 'g'")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     float g(int x, float y) {
       *       return 34.1f;
       *     }
       *   }
       * }}}
       */
      "check types that are compatible" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.f32Type),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x"), ArgDecl(TypeRef(typeNames.f32Type), "y")),
            Block(
              FloatVal(34.1f)
            )
          )
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            block.statements(0).asInstanceOf[MethodDecl].body.statements(0).evalType.name mustBe (typeNames.f32Type)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "variable declaration" should {

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
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block   = ast.asInstanceOf[Block]
            val varDecl = block.statements(0).asInstanceOf[VarDecl]
            varDecl.evalType.name mustBe (typeNames.voidType)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   globals
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
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block      = ast.asInstanceOf[Block]
            val assignment = block.statements(1).asInstanceOf[Assign]
            assignment.evalType.name mustBe (typeNames.voidType)

          case Left(t) => fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     float a = "A";
       *   }
       * }}}
       */
      "report an error if types are incompatible" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.f32Type), "a", StrVal("A"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot convert type 'string' to 'float' in variable 'a' declaration")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     float a = 1;
       *   }
       * }}}
       */
      "given no error if types are compatible" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.f32Type), "a", IntVal(1))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block  = ast.asInstanceOf[Block]
            val intVal = block.statements(0).asInstanceOf[VarDecl].expr.asInstanceOf[IntVal]
            intVal.evalType.name mustBe (typeNames.i32Type)
            intVal.promoteToType.map(_.name).get mustBe (typeNames.f32Type)

          case Left(t) => fail("Should be 'right", t)
      }
    }

    "if statement" should {

      /**
       * {{{
       *   // globals
       *   {
       *     void f() { }
       *
       *     int i = 0;
       *     int j = 0;
       *
       *     if ( 10 ) f();
       *   }
       * }}}
       */
      "fail if there no bool in the condition" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block()
          ),
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0)),
          VarDecl(TypeRef(typeNames.i32Type), "j", IntVal(0)),
          If(IntVal(10), Call(SymbolRef("f"), List.empty[Expr]))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Operation if('int') is not supported")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     void f() { }
       *
       *     int i = 0;
       *     int j = 0;
       *
       *     if ( i < j ) f();
       *   }
       * }}}
       */
      "type check if there is bool in the condition" in {
        val t = Block(
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List.empty[ArgDecl],
            Block()
          ),
          VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0)),
          VarDecl(TypeRef(typeNames.i32Type), "j", IntVal(0)),
          If(Less(Var(SymbolRef("i")), Var(SymbolRef("j"))), Call(SymbolRef("f"), List.empty[Expr]))
        )

        val errOrRes = eval(t)
        errOrRes.isRight mustBe (true)
      }

      /**
       * {{{
       *   // globals
       *   if(10 < 10) { 1 } else { 2.0 } // promoted to 'double'
       * }}}
       */
      "select type of the 'if' expression when a common type then(..) and else(..) can be found" in {
        val t = If(Less(IntVal(10), IntVal(10)), LongVal(1), Some(DoubleVal(2.0)))

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            ast.asInstanceOf[If].evalType.name mustBe (typeNames.f64Type)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     if(10 < 10) { "alice" } else { 2.0 }
       *   }
       * }}}
       */
      "report an error when common type for then(..) and else(..) cannot be found" in {
        val t = Block(
          If(Less(IntVal(10), IntVal(10)), StrVal("alice"), Some(DoubleVal(2.0)))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot find a common type for 'string' and 'double' in if")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     bool x = true;
       *     if(x) {
       *       return 4;
       *     } else {
       *       return 9;
       *     }
       *   }
       * }}}
       *
       * NOTE: here the eval-type of if-condition is 'int'
       */
      "deduce types with nested blocks" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.boolType), "x", BoolVal(true)),
          If(Var(SymbolRef("x")), Block(IntVal(4)), Some(Block(IntVal(9))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            ast.asInstanceOf[Block].evalType.name mustBe (typeNames.i32Type)
          case Left(t) =>
            fail("Should be 'left", t)
      }
    }

    "block" should {

      /**
       * {{{
       *   // globals
       *   {
       *   }
       * }}}
       */
      "evaluate to 'void' there are no items in the block" in {
        val t = Block()

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            ast.asInstanceOf[Block].evalType.name mustBe (typeNames.voidType)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     10;
       *     "alice";
       *     12.34;
       *   }
       * }}}
       */
      "evaluate to the type of the last expression" in {
        val t = Block(
          IntVal(10),
          StrVal("alice"),
          DoubleVal(12.34)
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            ast.asInstanceOf[Block].evalType.name mustBe (typeNames.f64Type)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "auto var declaration" should {

      /**
       * {{{
       *   // globals
       *   auto x = 2 + 3.0;
       * }}}
       */
      "deduce the type from an expression" in {
        val t        = VarDecl(TypeRef(typeNames.autoType), "x", Add(IntVal(2), DoubleVal(3.0)))
        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val varDecl = ast.asInstanceOf[VarDecl]
            varDecl.vType.name mustBe (typeNames.f64Type)
            varDecl.evalType.name mustBe (typeNames.voidType)
          case Left(t) =>
            fail("Should be 'right", t)
      }

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
      "be initialized" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.autoType), "x", BoolVal(true)),
          VarDecl(TypeRef(typeNames.autoType), "y", LongVal(4L)),
          VarDecl(TypeRef(typeNames.autoType), "s", StrVal("alice"))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block    = ast.asInstanceOf[Block]
            val xVarDecl = block.statements(0).asInstanceOf[VarDecl]
            val yVarDecl = block.statements(1).asInstanceOf[VarDecl]
            val sVarDecl = block.statements(2).asInstanceOf[VarDecl]

            xVarDecl.evalType.name mustBe (typeNames.voidType)
            yVarDecl.evalType.name mustBe (typeNames.voidType)
            sVarDecl.evalType.name mustBe (typeNames.voidType)

            xVarDecl.vType.name mustBe (typeNames.boolType)
            yVarDecl.vType.name mustBe (typeNames.i64Type)
            sVarDecl.vType.name mustBe (typeNames.strType)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "assignment" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int a = 0;
       *     a = 12.34;
       *   }
       * }}}
       */
      "report an error if types are incompatible" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "a", IntVal(0)),
          Assign(Var(SymbolRef("a")), DoubleVal(12.34))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot convert type 'double' to 'int' in the assignment")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int a = 0;
       *     a = 100;
       *   }
       * }}}
       */
      "report no errors if types are compatible" in {
        val t = Block(
          VarDecl(TypeRef(typeNames.i32Type), "a", IntVal(0)),
          Assign(Var(SymbolRef("a")), IntVal(100))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            block.evalType.name mustBe (typeNames.voidType)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "collection" should {

      /**
       * {{{
       *   // globals
       *   {
       *     int[] a = ["1", true, 3];
       *   }
       * }}}
       */
      "report an error if an element has incompatible type" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(StrVal("1"), BoolVal(true), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Element of the collection 'boolean' is not compatible with the collection type 'string'")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int[] a = [1.0f, 2.0f, 3.0f];
       *   }
       * }}}
       */
      "report an error if collection is not compatible with the var in assignment" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(FloatVal(1.0f), FloatVal(2.0f), FloatVal(3.0f))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(_) => fail("Should be 'left")
          case Left(t)  => t.getMessage mustBe ("Cannot convert type '[]float' to '[]int' in variable 'a' declaration")
      }

      /**
       * {{{
       *   // globals
       *   {
       *     int[] a = [1, 2, 3];
       *   }
       * }}}
       */
      "report no errors if all types are the same" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.i32Type)), "a", Vec(Seq(IntVal(1), IntVal(2), IntVal(3))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            block.statements(0).asInstanceOf[VarDecl].expr.evalType.name mustBe (s"[]${typeNames.i32Type}")
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     double[] a = [1, 2.3f, 4.5];
       *   }
       * }}}
       */
      "report no errors if types can be converted to the common one" in {
        val t = Block(
          VarDecl(VectorType(TypeRef(typeNames.f64Type)), "a", Vec(Seq(IntVal(1), FloatVal(2.3f), DoubleVal(4.5))))
        )

        val errOrRes = eval(t)
        errOrRes match
          case Right(TypeCheckVisitorState(ast, meta)) =>
            val block = ast.asInstanceOf[Block]
            block.statements(0).asInstanceOf[VarDecl].expr.evalType.name mustBe (s"[]${typeNames.f64Type}")
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "there is a program" should {

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
      "type check it" in {
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
          case Right(TypeCheckVisitorState(ast, meta)) =>
            // f
            val (fMethod, _) = findSMethodAST(meta, "f").value
            meta.retTypeFor(fMethod).map(_.name).value mustBe (typeNames.voidType)

            // g
            val (gMethod, _) = findSMethodAST(meta, "g").value
            meta.retTypeFor(gMethod).map(_.name).value mustBe (typeNames.voidType)

            // main
            val (mainMethod, _) = findSMethodAST(meta, "main").value
            meta.retTypeFor(mainMethod).map(_.name).value mustBe (typeNames.voidType)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phase 1, 2, 3
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve symbols that were populated in Phase-1
   *   - In Phase 3 we resolve types; after this phase all types (evalType, promoteToType) were resolved and we can evaluate the AST.
   */
  private def eval(ast0: AST): Either[Throwable, TypeCheckVisitorState] =
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
            val ss2  = s21.meta
            val ast2 = s21.ast

            // { AST->Scope } size must be the same before and after Phase #2
            s11.meta.astScopes.size mustEqual (s21.meta.astScopes.size)

            val typeCheckLaws = BTypeCheckLaws.make(types)

            val v3 = TypeCheckVisitor.make(types, typeCheckLaws)
            val s3 = TypeCheckState.make(ast2, s21.meta)
            ast2
              .visit(s3, v3)
              .flatMap({ s31 =>
                val t    = TypeCheckVisitorState(meta = s31.meta, ast = s31.ast)
                val ast3 = s31.ast

                // println(t.meta.show())

                // { AST->Scope } size must be the same before and after Phase #3
                s11.meta.astScopes.size mustEqual (s31.meta.astScopes.size)

                ast3
                  .visit(".", verifyTyped())
                  .flatMap(_ => verifyStateTypes(t.meta))
                  .map(_ => t)
              })
          }
      }

object TypeCheckVisitorSpec:

  final case class TypeCheckVisitorState(ast: AST, meta: Meta)

  def verifyTyped(): TreeVisitor[String, Unit] = new TreeVisitor[String, Unit]:

    override def visit(s: String, n: Init): Either[Throwable, Unit] = for
      _ <- checkType(n.iType, s"${s} -> Init")
      _ <- checkType(n.evalType, s"${s} -> Init(evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Init(promote)")
    yield ()

    override def visit(s: String, n: UnaryMinus): Either[Throwable, Unit] = for
      _ <- n.expr.visit(s"${s} -> UnaryMinus(expr", this)
      _ <- checkType(n.evalType, s"${s} -> UnaryMinus(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> UnaryMinus(... (promote)")
    yield ()

    override def visit(s: String, n: Add): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Add(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Add(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Add(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Add(... (promote)")
    yield ()

    override def visit(s: String, n: Sub): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Sub(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Sub(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Sub(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Sub(... (promote)")
    yield ()

    override def visit(s: String, n: Mul): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Mul(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mul(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Mul(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Mul(... (promote)")
    yield ()

    override def visit(s: String, n: Div): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Div(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Div(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Div(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Div(... (promote)")
    yield ()

    override def visit(s: String, n: Mod): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Mod(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Mod(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Mod(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Mod(... (promote)")
    yield ()

    override def visit(s: String, n: Less): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Less(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Less(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Less(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Less(... (promote)")
    yield ()

    override def visit(s: String, n: LessEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> LessEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> LessEqual(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> LessEqual(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> LessEqual(... (promote)")
    yield ()

    override def visit(s: String, n: Greater): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Greater(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Greater(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Greater(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Greater(... (promote)")
    yield ()

    override def visit(s: String, n: GreaterEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> GreaterEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> GreaterEqual(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> GreaterEqual(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> GreaterEqual(... (promote)")
    yield ()

    override def visit(s: String, n: Equal): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> equal(lhs", this)
      _ <- n.rhs.visit(s"${s} -> equal(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> equal(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> equal(... (promote)")
    yield ()

    override def visit(s: String, n: NotEqual): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> NotEqual(lhs", this)
      _ <- n.rhs.visit(s"${s} -> NotEqual(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> NotEqual(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> NotEqual(... (promote)")
    yield ()

    override def visit(s: String, n: Not): Either[Throwable, Unit] = for
      _ <- n.expr.visit(s"${s} -> Not(expr", this)
      _ <- checkType(n.evalType, s"${s} -> Not(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Not(... (promote)")
    yield ()

    override def visit(s: String, n: And): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> And(lhs", this)
      _ <- n.rhs.visit(s"${s} -> And(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> And(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> And(... (promote)")
    yield ()

    override def visit(s: String, n: Or): Either[Throwable, Unit] = for
      _ <- n.lhs.visit(s"${s} -> Or(lhs", this)
      _ <- n.rhs.visit(s"${s} -> Or(lhs, rhs", this)
      _ <- checkType(n.evalType, s"${s} -> Or(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Or(... (promote)")
    yield ()

    override def visit(s: String, n: Assign): Either[Throwable, Unit] = for
      _ <- n.id.visit(s"${s} -> Assign(id", this)
      _ <- n.expr.visit(s"${s} -> Assign(id, expr", this)
      _ <- checkType(n.evalType, s"${s} -> Assign(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Assign(... (promote)")
    yield ()

    override def visit(s: String, n: NothingVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> NothingVal() (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> NothingVal() (promote)")
    yield ()

    override def visit(s: String, n: VoidVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> VoidVal() (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> VoidVal() (promote)")
    yield ()

    override def visit(s: String, n: BoolVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> BoolVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> BoolVal(...) (promote)")
    yield ()

    override def visit(s: String, n: IntVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> IntVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> IntVal(...) (promote)")
    yield ()

    override def visit(s: String, n: LongVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> LongVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> LongVal(...) (promote)")
    yield ()

    override def visit(s: String, n: FloatVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> FloatVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> FloatVal(...) (promote)")
    yield ()

    override def visit(s: String, n: DoubleVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> DoubleVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> DoubleVal(...) (promote)")
    yield ()

    override def visit(s: String, n: DecimalVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> DecimalVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> DecimalVal(...) (promote)")
    yield ()

    override def visit(s: String, n: StrVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> StrVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> StrVal(...) (promote)")
    yield ()

    override def visit(s: String, n: DateVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> DateVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> DateVal(...) (promote)")
    yield ()

    override def visit(s: String, n: DateTimeVal): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> DateTimeVal(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> DateTimeVal(...) (promote)")
    yield ()

    override def visit(s: String, n: Vec): Either[Throwable, Unit] = for
      _ <- Transform.sequence(n.elements.zipWithIndex.map { case (expr, i) => expr.visit(s"${s} -> col(${i})", this) }).map(_ => ())
      _ <- checkType(n.evalType, s"${s} -> ? Vec(...) (evalType)")
      _ <- checkType(n.elementType, s"${s} -> Vec(?, ?, ?) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Vec(...) (promote)")
    yield ()

    override def visit(s: String, n: Var): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> ? Var(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Var(...) (promote)")
    yield ()

    override def visit(s: String, n: ArgDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.aType, s"${s} -> ArgDecl(${n.symbol.name}")
      _ <- checkType(n.evalType, s"${s} -> ArgDecl(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> ArgDecl(... (promote)")
    yield ()

    override def visit(s: String, n: VarDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.vType, s"${s} -> VarDecl(${n.symbol.name}")
      _ <- n.expr.visit(s"${s} -> VarDecl(${n.vType.name} ${n.name} = expr", this)
      _ <- checkType(n.evalType, s"${s} -> VarDecl(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> VarDecl(... (promote)")
    yield ()

    override def visit(s: String, n: FieldDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.fType, s"${s} -> FieldDecl(${n.symbol.name}")
      _ <- checkType(n.evalType, s"${s} -> FieldDecl(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> FieldDecl(... (promote)")
    yield ()

    override def visit(s: String, n: MethodDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.retType, s"${s} -> MethodDecl(${n.symbol.name}) -> retType")
      _ <- checkType(n.evalType, s"${s} -> MethodDecl(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> MethodDecl(... (promote)")
      _ <- Transform.sequence(n.params.map(p => p.visit(s"${s} -> MethodDecl(${n.name}(${p.name})", this)))
      _ <- n.body.visit(s"${s} -> fn ${n.name}(...) {", this)
    yield ()

    override def visit(s: String, n: StructDecl): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> StructDecl(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> StructDecl(... (promote)")
      _ <- Transform.sequence(n.fields.map(p => p.visit(s"${s} -> StructDecl(${n.name} {${p.name}}", this)))
    yield ()

    override def visit(s: String, n: Block): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> Block(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Block(... (promote)")
      _ <- Transform.sequence(n.statements.map(st => st.visit(s"${s} -> Block", this))).map(_ => ())
    yield ()

    override def visit(s: String, n: Call): Either[Throwable, Unit] = for
      _ <- Transform.sequence(n.args.map(e => e.visit(s"${s} -> Call(${n.id.name}(...)", this)))
      _ <- checkType(n.evalType, s"${s} -> ? Call(...) (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Call(...) (promote)")
    yield ()

    override def visit(s: String, n: If): Either[Throwable, Unit] = for
      _ <- n.cond.visit(s"${s} -> If(...)", this)
      _ <- n.then1.visit(s"${s} -> If(...) then { ", this)
      _ <- Transform.sequence(n.else1.map(_.visit(s"${s} -> If(...) then { ... } else {", this)))
      _ <- checkType(n.evalType, s"${s} -> If(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> If(... (promote)")
    yield ()

    override def visit(s: String, n: Access): Either[Throwable, Unit] = for
      _ <- n.a.visit(s"${s} -> Access(a", this)
      _ <- n.b.visit(s"${s} -> Access(a, b)", this)
      _ <- checkType(n.evalType, s"${s} -> Access(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> Access(... (promote)")
    yield ()

    override def visit(s: String, n: CompiledExpr): Either[Throwable, Unit] = for
      _ <- checkType(n.evalType, s"${s} -> CompiledExpr(... (evalType)")
      _ <- checkPromoteToType(n.promoteToType, s"${s} -> CompiledExpr(... (promote)")
    yield ()

    private def checkPromoteToType(ot: Option[Type], msg: String): Either[Throwable, Unit] =
      ot.map(t => checkType(t, msg)).fold(Right(()): Either[Throwable, Unit])(identity[Either[Throwable, Unit]])

  def verifyStateTypes(ss: Meta): Either[Throwable, Unit] =
    for
      _ <- Transform.sequence(ss.methodRetTypes.toList.map { case (ms, t) =>
             checkType(t, s"SMethod: ${ms.value.name}")
           })
      _ <- Transform.sequence(ss.varTypes.toList.map { case (ms, t) =>
             checkType(t, s"SVar: ${ms.value.name}")
           })
      _ <- Transform.sequence(ss.methodAsts.toList.map { case (ms, ast) =>
             ast.visit(s"verifying AST for (${ms.value.name}) -> .", verifyTyped())
           })
    yield ()

  /**
   * Checks that the type was resolved
   */
  def checkType(t: Type, msg: String): Either[Throwable, Unit] =
    for
      _ <- Either.cond(!t.isInstanceOf[TypeRef], (), new AstException(s"TypeRef '${t.name}' was found [${msg}]"))
      _ <- Either.cond((t.name != Type.Undefined.name), (), new AstException(s"Type '${t.name}' must be defined, but 'undefined' was found: [${msg}]."))
    yield ()
