package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.util.ResourceOps.resourceToString
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, SymbolRef, TypeRef }
import com.github.gchudnov.bscript.builder.AstMeta
import com.github.gchudnov.bscript.interpreter.memory.{ IntCell, StructCell }
import com.github.gchudnov.bscript.interpreter.memory.{ CellPath, Diff }
import com.github.gchudnov.bscript.inspector.internal.dbglib.{ MemWatchDiff, MemWatchStashEntry }
import com.github.gchudnov.bscript.translator.Lang

final class B1Spec extends TestSpec:
  private val typeNames = B1.typeNames

  "B1" when {
    val astA = VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0))
    val astB = Block(
      VarDecl(TypeRef(typeNames.autoType), "x", IntVal(10)),
      VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
      Var(SymbolRef("y"))
    )

    "AST is deserialized" should {
      "produce AST" in {
        val input = resourceToString("data/var-decl-ast.json").toTry.get

        val expected = astA

        val errOrRes = B1.load(input)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is serialized" should {
      "produce JSON" in {
        val t = astA

        val expected = resourceToString("data/var-decl-ast.json").toTry.get

        val errOrRes = B1.save(t)
        errOrRes match
          case Right(actual) =>
            actual mustBe (expected)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is built" should {
      "produce a new AST" in {
        val ast0 = astB

        val errOrRes = B1.build(ast0, B1Options.default.withPrelude(false))
        errOrRes match
          case Right(AstMeta(ast1, meta1)) =>
            val block    = ast1.asInstanceOf[Block]
            val autoDecl = block.statements(0).asInstanceOf[VarDecl]
            val varDecl  = block.statements(1).asInstanceOf[VarDecl]
            val yVar     = block.statements(2).asInstanceOf[Var]

            autoDecl.evalType.name mustBe (typeNames.voidType)
            autoDecl.expr.evalType.name mustBe (typeNames.i32Type)
            varDecl.vType.name mustBe (typeNames.i32Type)
            yVar.evalType.name mustBe (typeNames.i32Type)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is interpreter" should {
      "produce the result" in {
        val ast0 = astB

        val errOrRes = B1.build(ast0).flatMap(B1.interpret)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(20)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is run" should {
      "build & interpret it" in {
        val ast0 = astB

        val errOrRes = B1.run(ast0)
        errOrRes match
          case Right(cell) =>
            cell mustBe IntCell(20)
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is translated" should {
      "produce Scala code without prelude" in {
        val ast0 = astB

        val errOrRes = B1.translate(ast0, B1Options.default.withLang(Lang.Scala3).withPrelude(false))
        errOrRes match
          case Right(code) =>
            code mustBe """{
                          |  var x: Int = 10
                          |  var y: Int = 20
                          |  y
                          |}""".stripMargin
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "produce Scala code with prelude" in {
        val ast0 = astB

        val errOrRes = B1.translate(ast0, B1Options.default.withLang(Lang.Scala3).withPrelude(true))
        errOrRes match
          case Right(code) =>
            code.contains("var y: Int = 20") mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }

      "produce Scala code with Java Types with prelude" in {
        val ast0 = astB

        val errOrRes = B1.translate(ast0, B1Options.default.withLang(Lang.Scala3J).withPrelude(true))
        errOrRes match
          case Right(code) =>
            code.contains("var y: JInteger = 20") mustBe true
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "AST is inspected" should {

      /**
       * {{{
       *   // globals
       *   {
       *      int x = 1;
       *
       *      void main() {
       *        x = 2;
       *      }
       *   }
       * }}}
       */
      "trace global variable" in {
        val ast0 = Block(
          VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "main",
            List.empty[ArgDecl],
            Block(
              Assign(
                Var(SymbolRef("y")),
                IntVal(2)
              )
            )
          ),
          Call(SymbolRef("main"), List.empty[Expr]),
          Var(SymbolRef("y"))
        )

        val expectedCell = IntCell(2)
        val expectedLog  = Vector(MemWatchDiff("main", CellPath("y"), List(Diff.Updated("y", IntCell(1), IntCell(2)))))

        val errOrRes = B1.debug("y", ast0)
        errOrRes match
          case Right((cell, log)) =>
            cell mustBe expectedCell
            log must contain allElementsOf expectedLog
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct A {
       *       int x;
       *       B b;
       *     };
       *     struct B { int y; };
       *
       *     A a;
       *
       *     void g(int x) { a.x = x; }
       *     void f(int x) { int y = 1; g(2*x); a.b.y = 2; }
       *     void main() { f(3); }
       *
       *     main();
       *     a;
       *   }
       * }}}
       */
      "trace the memory state between function calls with nested calls" in {
        val ast0 = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Assign(
                Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
                Var(SymbolRef("x"))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
              Call(SymbolRef("g"), List(Mul(IntVal(2), Var(SymbolRef("x"))))),
              Assign(
                Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
                IntVal(2)
              )
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
          Call(SymbolRef("main"), List.empty[Expr]),
          Var(SymbolRef("a"))
        )

        val expectedCell = StructCell(
          Map(
            "x" -> IntCell(6),
            "b" -> StructCell(Map("y" -> IntCell(2)))
          )
        )

        // NOTE: we have duplication of diff entries because the functions are nested: main() -> f() -> g()
        val expectedLog = Vector(
          MemWatchDiff("g", CellPath("a"), List(Diff.Updated("a.x", IntCell(0), IntCell(6)))),
          MemWatchDiff("f", CellPath("a"), List(Diff.Updated("a.x", IntCell(0), IntCell(6)), Diff.Updated("a.b.y", IntCell(0), IntCell(2)))),
          MemWatchDiff("main", CellPath("a"), List(Diff.Updated("a.x", IntCell(0), IntCell(6)), Diff.Updated("a.b.y", IntCell(0), IntCell(2))))
        )

        val errOrRes = B1.debug("a", ast0)
        errOrRes match
          case Right((cell, log)) =>
            cell mustBe expectedCell
            log must contain allElementsOf expectedLog
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct A {
       *       int x;
       *       B b;
       *     };
       *     struct B { int y; };
       *
       *     A a;
       *
       *     void f(int x) { a.b.y = x; }
       *     void g(int x) { a.x = x; }
       *     void h() { }
       *
       *     f(10);
       *     g(20);
       *     h();
       *   }
       * }}}
       */
      "trace the memory state between function calls with sequential calls" in {
        val ast0 = Block(
          StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
          StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
          VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "f",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Assign(
                Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
                Var(SymbolRef("x"))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "g",
            List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
            Block(
              Assign(
                Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
                Var(SymbolRef("x"))
              )
            )
          ),
          MethodDecl(
            TypeRef(typeNames.voidType),
            "h",
            List.empty[ArgDecl],
            Block(
            )
          ),
          Call(SymbolRef("f"), List(IntVal(10))),
          Call(SymbolRef("g"), List(IntVal(20))),
          Call(SymbolRef("h"), List.empty[Expr]),
          Var(SymbolRef("a"))
        )

        val expectedCell = StructCell(
          Map(
            "x" -> IntCell(20),
            "b" -> StructCell(Map("y" -> IntCell(10)))
          )
        )

        val expectedLog = Vector(
          MemWatchDiff("f", CellPath("a"), List(Diff.Updated("a.b.y", IntCell(0), IntCell(10)))),
          MemWatchDiff("g", CellPath("a"), List(Diff.Updated("a.x", IntCell(0), IntCell(20)))),
          MemWatchDiff("h", CellPath("a"), List())
        )

        val errOrRes = B1.debug("a", ast0)
        errOrRes match
          case Right((cell, log)) =>
            cell mustBe expectedCell
            log must contain allElementsOf expectedLog
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }
