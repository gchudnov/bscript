package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.translator.TestSpec
import com.github.gchudnov.bscript.lang.ast.*

final class Scala3ImportSpec extends TestSpec:

  final case class DataModel(
    var a: Int,
    var b: String,
    var c: String,
  )

  val r = DataModel(1, "", "EUR")

  "Scala3Import" when {
    "importing scala-code" should {
      
      "int constant" in {
        val actual = Scala3Import.make({
          10
        })
        val expected = IntVal(10)

        actual mustBe expected
      }

      "bool constant" in {
        val actual = Scala3Import.make({
          true
        })
        val expected = BoolVal(true)

        actual mustBe expected
      }
    }
  }

/*
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