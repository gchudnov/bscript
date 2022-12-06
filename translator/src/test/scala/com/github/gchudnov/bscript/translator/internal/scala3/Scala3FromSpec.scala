package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.translator.TestSpec

final class Scala3FromSpec extends TestSpec:

  final case class DataModel(
    var a: Int,
    var b: String,
    var c: String,
  )

  val r = DataModel(1, "", "EUR")

  "Scala3From" when {
    "scala code is converted to AST" should {
      "build AST" in {

        val a = 2
        val b = 3

        // Transpiler.debugSingle(a+b)
        // Transpiler.debug(1, 2, 3, a, b)

        // inline def code = myRule _

        Scala3From.make({
          10
          // r.a = 10
          // r.b = "Test"          
          // if (r.c == "HRK") then {
          //   r.a = 20
          // }
        })
        
      }
    }
  }

/*
{
  r.a = 10
  r.b = "Test"          
}

Block(List(Assign(Select(Select(This(Ident(TranspilerSpec)),r),a),Literal(Constant(10)))),Assign(Select(Select(This(Ident(TranspilerSpec)),r),b),Literal(Constant(Test))))


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