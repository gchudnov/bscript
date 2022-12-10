package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.translator.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

final class Scala3ImportSpec extends TestSpec:

  final case class DataModel(
    var a: Int,
    var b: String,
    var c: String
  )

  val r = DataModel(1, "", "EUR")

  "Scala3Import" when {
    "importing scala-code" should {

      "int constant" in {
        // IntConstant(10)
        val actual = Scala3Import.make({
          10
        })

        val expected = IntVal(10)

        actual mustBe expected
      }

      "bool constant" in {
        // BooleanConstant(true)
        val actual = Scala3Import.make({
          true
        })

        val expected = BoolVal(true)

        actual mustBe expected
      }

      "decl int val" in {
        // Block(List(ValDef("a", Inferred(), Some(Literal(IntConstant(10))))), Literal(UnitConstant()))
        val actual = Scala3Import.make({
          val a = 10
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "a", IntVal(10)),
          VoidVal()
        )

        actual mustBe expected
      }

      "decl int var" in {
        // Block(List(ValDef("b", Inferred(), Some(Literal(IntConstant(10))))), Literal(UnitConstant())))
        // NOTE: Scala AST looks exactly the same as we create a `val`
        val actual = Scala3Import.make({
          var b = 10
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "b", IntVal(10)),
          VoidVal()
        )
        actual mustBe expected
      }

      "decl list of constants" in {
        // Block(List(ValDef("x", Inferred(), Some(Apply(TypeApply(Select(Ident("List"), "apply"), List(Inferred())), List(Typed(Repeated(List(Literal(IntConstant(10)), Literal(IntConstant(20))), Inferred()), Inferred())))))), Literal(UnitConstant()))
        val actual = Scala3Import.make({
          var x = List(10, 20)
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "x", Vec(List(IntVal(10), IntVal(20)))),
          VoidVal()
        )
        actual mustBe expected
      }

      "list of strings" in {
        val actual = Scala3Import.make({
          List("a", "b")
        })

        val expected = Block(
          Vec(List(StrVal("a"), StrVal("b")))
        )

        actual mustBe expected
      }

      "list of variables" in {
        val actual = Scala3Import.make({
          val a = 1
          val b = 2
          List(a, b)
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "a", IntVal(1)),
          VarDecl(TypeRef("auto"), "b", IntVal(2)),
          Vec(List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "assign var" in {
        // Block(List(ValDef("c", Inferred(), Some(Literal(IntConstant(10))))), Assign(Ident("c"), Literal(IntConstant(30))))
        val actual = Scala3Import.make({
          var c = 10
          c = 30
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "c", IntVal(10)),
          Assign(Var(SymbolRef("c")), IntVal(30))
        )
        actual mustBe expected
      }

      "assign var defined in the outer scope" in {
        // Assign(Ident("d"), Literal(IntConstant(2))))
        var d = 1
        val actual = Scala3Import.make({
          d = 2
        })

        val expected = Block(
          Assign(Var(SymbolRef("d")), IntVal(2))
        )

        actual mustBe expected
      }

      "assign member of an outer struct" in {
        // Assign(Select(Ident("d"), "x"), Literal(IntConstant(100))))
        case class Data(var x: Int)
        val d = Data(10)

        val actual = Scala3Import.make({
          d.x = 100
        })

        val expected = Block(
          Assign(Access(Var(SymbolRef("d")), Var(SymbolRef("x"))), IntVal(100))
        )

        actual mustBe expected
      }

      "eq" in {
        // Apply(Select(Ident("a"), "=="), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a == b
        })

        val expected = Block(
          Call(SymbolRef("=="), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "neq" in {
        // Apply(Select(Ident("a"), "!="), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a != b
        })

        val expected = Block(
          Call(SymbolRef("!="), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "gt" in {
        // Apply(Select(Ident("a"), ">"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a > b
        })

        val expected = Block(
          Call(SymbolRef(">"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "lt" in {
        // Apply(Select(Ident("a"), "<"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a < b
        })

        val expected = Block(
          Call(SymbolRef("<"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "plus" in {
        // Apply(Select(Ident("a"), "+"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a + b
        })

        val expected = Block(
          Call(SymbolRef("+"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "minus" in {
        // Apply(Select(Ident("a"), "-"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a - b
        })

        val expected = Block(
          Call(SymbolRef("-"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "mul" in {
        // Apply(Select(Ident("a"), "*"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a * b
        })

        val expected = Block(
          Call(SymbolRef("*"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "div" in {
        // Apply(Select(Ident("a"), "/"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a / b
        })

        val expected = Block(
          Call(SymbolRef("/"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "mod" in {
        // Apply(Select(Ident("a"), "%"), List(Ident("b")))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          a % b
        })

        val expected = Block(
          Call(SymbolRef("%"), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))
        )

        actual mustBe expected
      }

      "assignment to comparison" in {
        // Block(List(ValDef("c", Inferred(), Some(Apply(Select(Ident("a"), "=="), List(Ident("b")))))), Literal(UnitConstant()))
        val a = 10
        val b = 20

        val actual = Scala3Import.make({
          val c = a == b
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "c", Call(SymbolRef("=="), List(Var(SymbolRef("a")), Var(SymbolRef("b"))))),
          VoidVal()
        )

        actual mustBe expected
      }

      "call method without args" in {
        def funcA(): Unit = {}

        val actual = Scala3Import.make({
          funcA()
        })

        val expected = Block(
          Call(SymbolRef("funcA"), List.empty[Expr])
        )

        actual mustBe expected
      }

      "call method with one arg" in {
        def funcA(x: Int): Unit = {}

        val actual = Scala3Import.make({
          funcA(12)
        })

        val expected = Block(
          Call(SymbolRef("funcA"), List(IntVal(12)))
        )

        actual mustBe expected
      }

      "call with string and collection" in {
        def contains(s: String, ss: List[String]) = {}

        val actual = Scala3Import.make({
          contains("a", List("b"))
        })

        val expected = Block(
          Call(SymbolRef("contains"), List(StrVal("a"), Vec(List(StrVal("b"))))),
        )

        actual mustBe expected
      }

      "if-then" in {
        // If(Apply(Select(Ident("x"), "=="), List(Literal(IntConstant(10)))), Block(Nil, Block(List(Literal(BooleanConstant(true))), Literal(UnitConstant()))), Literal(UnitConstant()))
        val x = 10

        val actual = Scala3Import.make({
          var a = 1
          if (x == 10) then a = 2
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "a", IntVal(1)),
          If(
            Call(SymbolRef("=="), List(Var(SymbolRef("x")), IntVal(10))),
            Assign(Var(SymbolRef("a")), IntVal(2)),
            Some(VoidVal())
          )
        )

        actual mustBe expected
      }

      "if-then-else" in {
        val x = 10

        val actual = Scala3Import.make({
          var a = 1
          if (x == 10) then a = 2 else a = 3
        })

        val expected = Block(
          VarDecl(TypeRef("auto"), "a", IntVal(1)),
          If(
            Call(SymbolRef("=="), List(Var(SymbolRef("x")), IntVal(10))),
            Assign(Var(SymbolRef("a")), IntVal(2)),
            Some(Assign(Var(SymbolRef("a")), IntVal(3)))
          )
        )

        actual mustBe expected
      }
    }
  }

/*
 * Scala3Import when importing scala-code should assignment to comparison - Block(List(VarDecl(ref auto, "c", Call(ref ==, ArraySeq(Var(ref a, type:UNDEFINED, None), Var(ref b, type:UNDEFINED, None)), type:UNDEFINED, None), symbol:UNDEFINED, type:UNDEFINED, None), VoidVal(type:UNDEFINED, None)), symbol:UNDEFINED, type:UNDEFINED, None) was not equal to Block(List(VarDecl(ref auto, "c", Call(ref ==, List(Var(ref a, type:UNDEFINED, None), Var(ref b, type:UNDEFINED, None)), type:UNDEFINED, None), symbol:UNDEFINED, type:UNDEFINED, None)), symbol:UNDEFINED, type:UNDEFINED, None)

{
  r.a = 10
  r.b = "Test"
}

Block(List(Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(10)))),Assign(Select(Select(This(Ident(TranspilerSpec)),r),b),Literal(Constant(Test))))


        val a = 2
        val b = 3

        // Transpiler.debugSingle(a+b)
        // Transpiler.debug(1, 2, 3, a, b)

        // inline def code = myRule _

          // r.a = 10
          // r.b = "Test"
          // if (r.c == "HRK") then {
          //   r.a = 20
          // }

{
          r.a = 10
          r.b = "Test"
          if (r.c == "HRK") then {
            r.a = 20
          }
        }

Block(List(Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(10))), Assign(Select(Select(This(Ident(TranspilerSpec)),r),b),Literal(Constant(Test)))),If(Apply(Select(Select(Select(This(Ident(TranspilerSpec)),r),c),==),List(Literal(Constant(HRK)))),Block(List(),Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(20)))),Literal(Constant(()))))


[info] compiling 1 Scala source to /home/gchudnov/Projects/bscript/transpiler/target/scala-3.2.1/classes ...
[info] compiling 1 Scala source to /home/gchudnov/Projects/bscript/transpiler/target/scala-3.2.1/test-classes ...
BLOCK:If(Apply(Select(Select(Select(This(Ident(TranspilerSpec)),r),c),==),List(Literal(Constant(HRK)))),Block(List(),Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(20)))),Literal(Constant(())))
HERE!Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(10)))
HERE!Assign(Select(Select(This(Ident(TranspilerSpec)),r),b),Literal(Constant(Test)))

we need to convert B1 AST to RE AST, then use this code

 */
