package com.github.gchudnov.bscript.interpreter.pass

import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.Examples
import com.github.gchudnov.bscript.builder.Builder
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.TestSpec

import scala.util.control.Exception.*
import com.github.gchudnov.bscript.interpreter.env.HasRetValue

import java.time.LocalDate
import java.time.OffsetDateTime

final class InterpretPassSpec extends TestSpec:
  import InterpretPassSpec.*

  "InterpretPass" when {

    "const literals" should {

      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "interpret an integer" in {
        val t = Examples.intVal

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(2))
            ()
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   2
       *   3
       * }}}
       */
      "interpret several integers" in {
        val t = Examples.twoInts

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(3))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   2
       *   {
       *     3
       *   }
       * }}}
       */
      "interpret several integers when block is inside a block" in {
        val t = Examples.blockNested

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(3))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   2
       *   {
       *     3
       *   }
       *   4
       * }}}
       */
      "interpret several integers when the last value is outside of a block" in {
        val t = Examples.blockInner

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(4))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "declaration" should {

      /**
       * Variable Declaration
       *
       * {{{
       *   // globals
       *   int x = 0;
       * }}}
       */
      "interpret x = 0" in {
        val t = Examples.varDef

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.Void)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   x;
       * }}}
       */
      "declare x, assign and return" in {
        val t = Examples.xDeclReturnX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(0))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = nothing;
       *   x;
       * }}}
       */
      "declare x, assign to nothing and return" in {
        val t = Examples.intNothingX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.Nothing)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * Allow nothing in assignment
       *
       * {{{
       *   // globals
       *   {
       *     int x = 0;
       *     x = nothing;
       *     x;
       *   }
       * }}}
       */
      "allow nothing in assignment" in {
        val t = Examples.assignNothingX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.Nothing)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   long y = 1;
       *   x;
       * }}}
       */
      "declare x, y, return x" in {
        val t = Examples.xyDeclReturnX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(0))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   long y = 1;
       *   y;
       * }}}
       */
      "declare x, y, return y" in {
        val t = Examples.xyDeclReturnY

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I64(1L))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   long y = 1;
       *   z;
       * }}}
       */
      "declare x, y, return z" in {
        val t = Examples.xyDeclReturnZ

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("'z' is not found")
      }

      /**
       * Variable Declaration & Assign, Return Void
       *
       * {{{
       *   // globals
       *   int x = 0;
       *   x = 1;
       * }}}
       */
      "declare x, assign x, return void" in {
        val t = Examples.xDeclAssign

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.Void)
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   auto x = 0; // shold be auto-deduced to type: i32
       *   x;
       * }}}
       */
      "declare auto x, return it" in {
        val t = Examples.autoDeclReturnX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(0))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   long y = 1;
       *   x = y;       // NOTE: y is not compatible with x
       * }}}
       */
      "assignment of uncompatible types" in {
        val t = Examples.xyDeclAssignUncompat

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("Type mismatch: BuiltInType(i32) != BuiltInType(i64)")
      }

      /**
       * {{{
       *   // globals
       *   int x = "abc";
       *   x;
       * }}}
       */
      "declaration and init of uncompatible types produces an error" in {
        val t = Examples.xyDeclWrongInit

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("Type mismatch: BuiltInType(i32) != BuiltInType(str) in the variable declaration")
      }

      /**
       * {{{
       *   // globals
       *   long x = _;
       *   x;
       * }}}
       */
      "declaration with init for an integer" in {
        val t = Examples.xDeclDfaultReturnX

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I64(0))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "conditions" should {

      /**
       * IF(true)
       *
       * {{{
       *   // globals
       *   {
       *     bool x = true;
       *     if(x) {
       *       4;
       *     } else {
       *       9;
       *     }
       *   }
       * }}}
       */
      "return value from IF-branch" in {
        val t = Examples.ifTrue

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(4))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * IF(false)
       *
       * {{{
       *   // globals
       *   {
       *     bool x = false;
       *     if(x) {
       *       4;
       *     } else {
       *       9;
       *     }
       *   }
       * }}}
       */
      "return value from ELSE-branch" in {
        val t = Examples.ifFalse

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.I32(9))
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "struct" should {

      /**
       * {{{
       *   // globals
       *   {
       *     struct A { };
       *     A a;
       *   }
       * }}}
       */
      "init an empty struct" in {
        val t = Examples.structEmpty

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.struct(Map.empty[String, Cell]))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * {{{
       *   // globals
       *   {
       *     struct A { int x; };
       *
       *     A a;
       *     a;
       *   }
       * }}}
       */
      "init a struct with one field" in {
        val t = Examples.structOneField

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.retValue mustBe (Cell.struct(Map("x" -> Cell.i32(0))))
          case Left(t) =>
            fail("Should be 'right", t)
      }

      // TODO: fixme: ^^^^
    }
  }

  /**
   * To evaluate, we Build that run Interpret Pass.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    val interpretPass = new InterpretPass()

    val (ast1, buildState) = Builder.build(ast0).toTry.get

    val interpretIn = new HasAST with HasReadEvalTypes:
      override val ast: AST                 = ast1
      override def evalTypes: ReadEvalTypes = buildState.evalTypes

    val interpretOut = interpretPass.run(interpretIn)
    val actualState  = toActualState(interpretOut)

    actualState
  }

object InterpretPassSpec:
  final case class ActualState(
    retValue: Cell,
  )

  def toActualState(s: HasRetValue): ActualState =
    ActualState(
      retValue = s.retValue,
    )

//     Builder
//       .build(ast0, types, typeCheckLaws)
//       .flatMap({ astMeta =>
//         val ms = MemorySpace("globals")

//         val laws = IInterpretLaws.make(types, astMeta.meta)

//         val interpretVisitor = InterpretVisitor.make(laws)
//         val interpretState   = InterpretState.make(meta = astMeta.meta, Stash.empty, memSpace = ms, retValue = VoidCell)

//         astMeta.ast.visit(interpretState, interpretVisitor)
//       })

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 1;
//        *     void g(int x) { int z = 2; }
//        *     void f(int x) { int y = 1; g(2*x); }
//        *     int main() { f(3); }
//        *     main();
//        *   }
//        * }}}
//        */
//       "interpret it" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(1)),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "g",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               VarDecl(TypeRef(typeNames.i32Type), "z", IntVal(2))
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "f",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
//               Call(SymbolRef("g"), List(Mul(IntVal(2), Var(SymbolRef("x")))))
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "main",
//             List.empty[ArgDecl],
//             Block(
//               Call(SymbolRef("f"), List(IntVal(3)))
//             )
//           ),
//           Call(SymbolRef("main"), List.empty[Expr])
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) =>
//             c mustBe (VoidCell)
//           case Left(t) =>
//             fail("Should be 'right", t)
//       }
//     }

//     "variable declaration" should {

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int a = 3;
//        *     a = a + 2;
//        *     a
//        *   }
//        * }}}
//        */
//       "allow self-assignment" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "a", IntVal(3)),
//           Assign(Var(SymbolRef("a")), Add(Var(SymbolRef("a")), IntVal(2))),
//           Var(SymbolRef("a"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(5)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   each case is run separately:
//        *     { int x = "11-"; }
//        *     { int x = true; }
//        *     { decimal x = "a.b"; }
//        *     { decimal x = false; }
//        *     { string x = 12; }
//        *     { bool x = "true"; }
//        *     { date x = "2019-AA-BB"; }
//        *     { datetime x = true; }
//        * }}}
//        */
//       "fail evaluation if incorrect types are provided" in {
//         val blocks = List(
//           Block(VarDecl(TypeRef(typeNames.i32Type), "x", StrVal("11-"))),
//           Block(VarDecl(TypeRef(typeNames.i32Type), "x", BoolVal(true))),
//           Block(VarDecl(TypeRef(typeNames.decType), "x", StrVal("a.b"))),
//           Block(VarDecl(TypeRef(typeNames.decType), "x", BoolVal(false))),
//           Block(VarDecl(TypeRef(typeNames.strType), "x", IntVal(12))),
//           Block(VarDecl(TypeRef(typeNames.boolType), "x", StrVal("true"))),
//           Block(VarDecl(TypeRef(typeNames.dateType), "x", StrVal("2019-AA-BB"))),
//           Block(VarDecl(TypeRef(typeNames.datetimeType), "x", BoolVal(true)))
//         )

//         blocks.foreach { t =>
//           val errOrRes = eval(t)
//           errOrRes match
//             case Right(_) => fail("Should be 'left")
//             case Left(t)  => t.getMessage.contains("Cannot convert") mustBe (true)
//         }
//       }

//       /**
//        * {{{
//        *   each case is run separately:
//        *     { int x = 123; }
//        *     { decimal x = decimal(1234567890); }
//        *     { string x = "alice"; }
//        *     { bool x = true; }
//        *     { date x = date("2021-10-04"); }
//        *     { datetime x = datetime("2021-10-04T12:35:45+02:00"); }
//        * }}}
//        */
//       "evaluate if types are matching" in {
//         val blocks = List(
//           Block(VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(123))),
//           Block(VarDecl(TypeRef(typeNames.decType), "x", DecimalVal(BigDecimal.valueOf(1234567890)))),
//           Block(VarDecl(TypeRef(typeNames.strType), "x", StrVal("alice"))),
//           Block(VarDecl(TypeRef(typeNames.boolType), "x", BoolVal(true))),
//           Block(VarDecl(TypeRef(typeNames.dateType), "x", DateVal(LocalDate.parse("2021-10-04")))),
//           Block(VarDecl(TypeRef(typeNames.datetimeType), "x", DateTimeVal(OffsetDateTime.parse("2021-10-04T12:35:45+02:00"))))
//         )

//         blocks.foreach { t =>
//           val errOrRes = eval(t)
//           errOrRes match
//             case Right(InterpretState(_, _, ms, c)) => c mustBe (VoidCell)
//             case Left(t)                            => fail("Should be 'right", t)
//         }
//       }
//     }

//     "if" should {

//       /**
//        * {{{
//        *   // globals
//        *   if(7) {
//        *     2 + 5;
//        *   }
//        * }}}
//        */
//       "report an error when condition is not boolean" in {
//         val t = If(IntVal(7), Add(IntVal(2), IntVal(5)))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(_) => fail("Should be 'left")
//           case Left(t)  => t.getMessage mustBe (s"Operation if('${typeNames.i32Type}') is not supported")
//       }

//       /**
//        * {{{
//        *   // globals
//        *   if(7 < 5) {
//        *     2 + 5;
//        *   } else {
//        *     if(1L + 2) {
//        *       // no-op
//        *     }
//        *   }
//        * }}}
//        */
//       "return an error when else-if condition is not boolean" in {
//         val t = If(Less(IntVal(7), IntVal(5)), Add(IntVal(2), IntVal(5)), Some(If(Add(LongVal(1L), IntVal(2)), Block())))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(_) => fail("Should be 'left")
//           case Left(t)  => t.getMessage mustBe (s"Operation if('${typeNames.i64Type}') is not supported")
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *
//        *     if(5.0 > 1) {
//        *       x = 1;
//        *     } else {
//        *       x = 2;
//        *     }
//        *
//        *     return x;
//        *   }
//        * }}}
//        */
//       "should update memory in then-clause" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           If(Greater(DoubleVal(5.0), IntVal(1)), Assign(Var(SymbolRef("x")), IntVal(1)), Some(Assign(Var(SymbolRef("x")), IntVal(2)))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(1)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *
//        *     if(5.0 > 10L) {
//        *       x = 1;
//        *     } else {
//        *       x = 2;
//        *     }
//        *
//        *     return x;
//        *   }
//        * }}}
//        * }}}
//        */
//       "should update memory in else-clause" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           If(Greater(DoubleVal(5.0), LongVal(10L)), Assign(Var(SymbolRef("x")), IntVal(1)), Some(Assign(Var(SymbolRef("x")), IntVal(2)))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(2)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "+" should {

//       /**
//        * {{{
//        *   // globals
//        *   1 + 2;
//        * }}}
//        */
//       "eval if operands have the same type" in {
//         val t = Add(IntVal(1), IntVal(2))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(3)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   "ali" + "ce";
//        * }}}
//        */
//       "eval if operands are strings" in {
//         val t = Add(StrVal("ali"), StrVal("ce"))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe StrCell("alice")
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   67L + nothing;
//        * }}}
//        */
//       "return an error if we add with nothing" in {
//         val t = Add(LongVal(67L), NothingVal())

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(_) => fail("Should be 'left")
//           case Left(t)  => t.getMessage mustBe (s"Operation +('${typeNames.i64Type}', '${typeNames.nothingType}') is not supported")
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     x = nothing;
//        *     82 + x;
//        *   }
//        * }}}
//        */
//       "return an error if we add with nothing indirectly" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", NothingVal()),
//           Add(IntVal(82), Var(SymbolRef("x")))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(_) => fail("Should be 'left")
//           case Left(t)  => t.getMessage mustBe (s"Cannot eval Add(IntCell(82), NothingCell)")
//       }
//     }

//     "-" should {

//       /**
//        * {{{
//        *   // globals
//        *   1 - 2;
//        * }}}
//        */
//       "eval if operands have the same type" in {
//         val t = Sub(IntVal(1), IntVal(2))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(-1)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "*" should {

//       /**
//        * {{{
//        *   // globals
//        *   2 * 3;
//        * }}}
//        */
//       "eval if operands have the same type" in {
//         val t = Mul(LongVal(2L), LongVal(3L))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe LongCell(6)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "/" should {

//       /**
//        * {{{
//        *   8.0 / 4.0;
//        * }}}
//        */
//       "eval if operands have the same type" in {
//         val t = Div(FloatVal(8.0f), FloatVal(4.0f))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe FloatCell(2.0f)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "%" should {

//       /**
//        * {{{
//        *   // globals
//        *   8 % 6;
//        * }}}
//        */
//       "eval if operands have the same type" in {
//         val t = Mod(IntVal(8), IntVal(6))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(2)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "&&" should {

//       /**
//        * {{{
//        *   // globals
//        *   true && true
//        * }}}
//        */
//       "eval true && true" in {
//         val t = And(BoolVal(true), BoolVal(true))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(true)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   true && false;
//        * }}}
//        */
//       "eval true && false" in {
//         val t = And(BoolVal(true), BoolVal(false))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(false)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   false && true;
//        * }}}
//        */
//       "eval false && true" in {
//         val t = And(BoolVal(false), BoolVal(true))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(false)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   false && false;
//        * }}}
//        */
//       "eval false && false" in {
//         val t = And(BoolVal(false), BoolVal(false))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(false)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *     false && { x = 1; false }
//        *   }
//        * }}}
//        */
//       "(a && b) if 'a' is 'false', do not eval 'b'" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           And(Block(BoolVal(false)), Block(Assign(Var(SymbolRef("x")), IntVal(1)), BoolVal(false))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(0)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *     true && { x = 1; false }
//        *   }
//        * }}}
//        */
//       "(a && b) if 'a' is 'true', eval 'b'" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           And(Block(BoolVal(true)), Block(Assign(Var(SymbolRef("x")), IntVal(1)), BoolVal(false))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(1)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "||" should {

//       /**
//        * {{{
//        *   // globals
//        *   true || true;
//        * }}}
//        */
//       "eval true || true" in {
//         val t = Or(BoolVal(true), BoolVal(true))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(true)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   true || false;
//        * }}}
//        */
//       "eval true || false" in {
//         val t = Or(BoolVal(true), BoolVal(false))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(true)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   false || true;
//        * }}}
//        */
//       "eval false || true" in {
//         val t = Or(BoolVal(false), BoolVal(true))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(true)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   false || false;
//        * }}}
//        */
//       "eval false || false" in {
//         val t = Or(BoolVal(false), BoolVal(false))

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(false)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *     false || { x = 1; false }
//        *   }
//        * }}}
//        */
//       "(a || b) if 'a' is 'false', eval 'b'" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           Or(Block(BoolVal(false)), Block(Assign(Var(SymbolRef("x")), IntVal(1)), BoolVal(false))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(1)
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 0;
//        *     true || { x = 1; false }
//        *   }
//        * }}}
//        */
//       "(a || b) if 'a' is 'true', do not eval 'b'" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(0)),
//           Or(Block(BoolVal(true)), Block(Assign(Var(SymbolRef("x")), IntVal(1)), BoolVal(false))),
//           Var(SymbolRef("x"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(0)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "bool expressions" should {

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     auto x = true;     // bool
//        *     auto y = 3;        // long
//        *     auto s = "alice";  // string
//        *     (((y + 1) >= 4) and x) or (s == "alice")
//        *   }
//        * }}}
//        */
//       "be evaluated with auto-declarations" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.autoType), "x", BoolVal(true)),
//           VarDecl(TypeRef(typeNames.autoType), "y", LongVal(4L)),
//           VarDecl(TypeRef(typeNames.autoType), "s", StrVal("alice")),
//           Or(
//             And(GreaterEqual(Add(Var(SymbolRef("y")), IntVal(1)), IntVal(4)), Var(SymbolRef("x"))),
//             Equal(Var(SymbolRef("s")), StrVal("alice"))
//           )
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe BoolCell(true)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "type-declarations" should {

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 10;
//        *     decltype(x) y = 20;
//        *
//        *     y; // 20 of type int
//        *   }
//        * }}}
//        */
//       "be allowed in variable declarations" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(10)),
//           VarDecl(DeclType(Var(SymbolRef("x"))), "y", IntVal(20)),
//           Var(SymbolRef("y"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe IntCell(20)
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "functions are called" should {

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int x = 1;
//        *     int f(int x) { int y = 1; return g(2*x + y); }
//        *     int g(int x) { return (x - 1); }
//        *     int main() { return f(3); }
//        *     main();
//        *   }
//        * }}}
//        */
//       "evaluate the result for a chain of calls" in {
//         val t = Block(
//           VarDecl(TypeRef(typeNames.i32Type), "x", IntVal(1)),
//           MethodDecl(
//             TypeRef(typeNames.i32Type),
//             "f",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
//               Call(SymbolRef("g"), List(Add(Mul(IntVal(2), Var(SymbolRef("x"))), Var(SymbolRef("y")))))
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.i32Type),
//             "g",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               Sub(Var(SymbolRef("x")), IntVal(1))
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.i32Type),
//             "main",
//             List.empty[ArgDecl],
//             Block(
//               Call(SymbolRef("f"), List(IntVal(3)))
//             )
//           ),
//           Call(SymbolRef("main"), List.empty[Expr])
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(6))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     struct A {
//        *       int x;
//        *       B b;
//        *     };
//        *     struct B { int y; };
//        *
//        *     A a;
//        *
//        *     void g(int x) { a.x = x; }
//        *     void f(int x) { int y = 1; g(2*x); a.b.y = 2; }
//        *     void main() { f(3); }
//        *
//        *     main();
//        *     a;
//        *   }
//        * }}}
//        */
//       "evaluate the result when struct fields are assigned in functions" in {
//         import com.github.gchudnov.bscript.builder.state.Meta.*

//         val t = Block(
//           StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
//           StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
//           VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "g",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               Assign(
//                 Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
//                 Var(SymbolRef("x"))
//               )
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "f",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(1)),
//               Call(SymbolRef("g"), List(Mul(IntVal(2), Var(SymbolRef("x"))))),
//               Assign(
//                 Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
//                 IntVal(2)
//               )
//             )
//           ),
//           MethodDecl(
//             TypeRef(typeNames.voidType),
//             "main",
//             List.empty[ArgDecl],
//             Block(
//               Call(SymbolRef("f"), List(IntVal(3)))
//             )
//           ),
//           Call(SymbolRef("main"), List.empty[Expr]),
//           Var(SymbolRef("a"))
//         )

//         val expected = StructCell(
//           Map(
//             "x" -> IntCell(6),
//             "b" -> StructCell(Map("y" -> IntCell(2)))
//           )
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(meta, _, ms, c)) =>
//             c mustBe expected

//           case Left(t) =>
//             fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     int f(int x) {
//        *       if(x > 0) {
//        *         return x + f(x - 1);
//        *       }
//        *       else {
//        *         return 0;
//        *       }
//        *     }
//        *
//        *     f(4); // 4 + (3 + (2 + (1 + (0)))) = 10
//        * }
//        * }}}
//        */
//       "evaluate the result for a recursive function" in {
//         val t = Block(
//           MethodDecl(
//             TypeRef(typeNames.i32Type),
//             "f",
//             List(ArgDecl(TypeRef(typeNames.i32Type), "x")),
//             Block(
//               If(
//                 Greater(Var(SymbolRef("x")), IntVal(0)),
//                 Add(
//                   Var(SymbolRef("x")),
//                   Call(SymbolRef("f"), List(Sub(Var(SymbolRef("x")), IntVal(1))))
//                 ),
//                 Some(IntVal(0))
//               )
//             )
//           ),
//           Call(SymbolRef("f"), List(IntVal(4)))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(10))
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     "structs" should {

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     struct A {
//        *       int x;
//        *       B b;
//        *     };
//        *     struct B { int y; };
//        *
//        *     int i = 0;
//        *     int j = 0;
//        *
//        *     A a;
//        *     a.x = 1 + (i * j);
//        *     a.b.y = 2;
//        *
//        *     a.x + a.b.y; // should return 3
//        * }
//        * }}}
//        */
//       "produce a value when fields are added together" in {
//         val t = Block(
//           StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef("B"), "b"))),
//           StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
//           VarDecl(TypeRef(typeNames.i32Type), "i", IntVal(0)),
//           VarDecl(TypeRef(typeNames.i32Type), "j", IntVal(0)),
//           VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
//           Assign(
//             Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
//             Add(IntVal(1), Mul(Var(SymbolRef("i")), Var(SymbolRef("j"))))
//           ),
//           Assign(
//             Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y"))),
//             IntVal(2)
//           ),
//           Add(
//             Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
//             Access(Access(Var(SymbolRef("a")), Var(SymbolRef("b"))), Var(SymbolRef("y")))
//           )
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(3))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     struct A {
//        *       int x;
//        *       long z;
//        *       B b;
//        *     };
//        *     struct B { int y; };
//        *
//        *     A a;
//        *     a.x = 5;
//        *     a.z = a.x;
//        *
//        *     a.z
//        * }
//        * }}}
//        */
//       "allow assign fields to fields" in {
//         val t = Block(
//           StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.i64Type), "z"), FieldDecl(TypeRef("B"), "b"))),
//           StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
//           VarDecl(TypeRef("A"), "a", Init(TypeRef("A"))),
//           Assign(
//             Access(Var(SymbolRef("a")), Var(SymbolRef("x"))),
//             IntVal(5)
//           ),
//           Assign(
//             Access(Var(SymbolRef("a")), Var(SymbolRef("z"))),
//             Access(Var(SymbolRef("a")), Var(SymbolRef("x")))
//           ),
//           Access(Var(SymbolRef("a")), Var(SymbolRef("z")))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (LongCell(5))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   {
//        *     struct B { int y; };
//        *
//        *     struct A {
//        *       int x;
//        *       string s;
//        *       B b;
//        *     };
//        *
//        *     A a = { 1, "hello", { 2 } };
//        *     a
//        *   }
//        * }}}
//        */
//       "initialize with an anonymous struct" in {
//         val t = Block(
//           StructDecl("B", List(FieldDecl(TypeRef(typeNames.i32Type), "y"))),
//           StructDecl("A", List(FieldDecl(TypeRef(typeNames.i32Type), "x"), FieldDecl(TypeRef(typeNames.strType), "s"), FieldDecl(TypeRef("B"), "b"))),
//           VarDecl(
//             TypeRef("A"),
//             "a",
//             StructVal(
//               TypeRef("A"),
//               Map(
//                 "x" -> IntVal(1),
//                 "s" -> StrVal("alice"),
//                 "b" -> StructVal(
//                   TypeRef("B"),
//                   Map(
//                     "y" -> IntVal(2)
//                   )
//                 )
//               )
//             )
//           ),
//           Var(SymbolRef("a"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) =>
//             c mustBe (StructCell(
//               Map(
//                 "x" -> IntCell(1),
//                 "s" -> StrCell("alice"),
//                 "b" -> StructCell(
//                   Map("y" -> IntCell(2))
//                 )
//               )
//             ))
//           case Left(t) => fail("Should be 'right", t)
//       }

//     }

//     "there is a program that uses compiled functions" should {

//       /**
//        * {{{
//        *   // globals
//        *   // prelude
//        *   {
//        *     string s = "str";
//        *     strlen(s);
//        *   }
//        * }}}
//        */
//       "strLen" in {
//         val t = IGlobals.prelude ++ Block(
//           VarDecl(TypeRef(typeNames.strType), "s", StrVal("str")),
//           Call(SymbolRef("strLen"), List(Var(SymbolRef("s"))))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(3))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   // prelude
//        *   {
//        *     datetime d1 = "2021-10-04T00:00:00+02:00";
//        *     datetime d2 = offsetDateTime(d, 12, "hours");
//        *     d2; // "2021-10-04T12:00+02:00"
//        *   }
//        * }}}
//        */
//       "offsetDateTime" in {
//         val t = IGlobals.prelude ++ Block(
//           VarDecl(TypeRef(typeNames.datetimeType), "d1", DateTimeVal(OffsetDateTime.parse("2021-10-04T00:00:00+02:00"))),
//           VarDecl(TypeRef(typeNames.datetimeType), "d2", Call(SymbolRef("offsetDateTime"), List(Var(SymbolRef("d1")), IntVal(12), StrVal("hours")))),
//           Var(SymbolRef("d2"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (DateTimeCell(OffsetDateTime.parse("2021-10-04T12:00+02:00")))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   // prelude
//        *   {
//        *     datetime d1 = "2021-10-04T01:00:00+02:00";
//        *     datetime d2 = offsetDateTime(d, 17, "hours");
//        *     d2;
//        *   }
//        * }}}
//        */
//       "setDateTime" in {
//         val t = IGlobals.prelude ++ Block(
//           VarDecl(TypeRef(typeNames.datetimeType), "d1", DateTimeVal(OffsetDateTime.parse("2021-10-04T01:00:00+02:00"))),
//           VarDecl(TypeRef(typeNames.datetimeType), "d2", Call(SymbolRef("setDateTime"), List(Var(SymbolRef("d1")), IntVal(17), StrVal("hours")))),
//           Var(SymbolRef("d2"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (DateTimeCell(OffsetDateTime.parse("2021-10-04T17:00+02:00")))
//           case Left(t)                            => fail("Should be 'right", t)
//       }

//       /**
//        * {{{
//        *   // globals
//        *   // prelude
//        *   {
//        *     datetime d1 = "YYYY-MM-DD HH:MM:SS";
//        *     int dd = fieldDateTime(d1, "days");
//        *     dd;
//        *   }
//        * }}}
//        */
//       "fieldOfDateTime" in {
//         val t = IGlobals.prelude ++ Block(
//           VarDecl(TypeRef(typeNames.datetimeType), "d1", DateTimeVal(OffsetDateTime.parse("2021-10-05T00:00:00+02:00"))),
//           VarDecl(TypeRef(typeNames.i32Type), "dd", Call(SymbolRef("fieldOfDateTime"), List(Var(SymbolRef("d1")), StrVal("days")))),
//           Var(SymbolRef("dd"))
//         )

//         val errOrRes = eval(t)
//         errOrRes match
//           case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(5))
//           case Left(t)                            => fail("Should be 'right", t)
//       }
//     }

//     /**
//      * {{{
//      *   // globals
//      *   // prelude
//      *   {
//      *     int x = null;
//      *     bool xr = isDefined(x); // false
//      *     xr;
//      *   }
//      * }}}
//      */
//     "isDefined for Nothing return false" in {
//       val t = IGlobals.prelude ++ Block(
//         VarDecl(TypeRef(typeNames.i32Type), "x", NothingVal()),
//         VarDecl(TypeRef(typeNames.boolType), "xr", Call(SymbolRef("isDefined"), List(Var(SymbolRef("x"))))),
//         Var(SymbolRef("xr"))
//       )

//       val errOrRes = eval(t)
//       errOrRes match
//         case Right(InterpretState(_, _, ms, c)) =>
//           c mustBe BoolCell(false)
//         case Left(t) => fail("Should be 'right", t)
//     }

//     /**
//      * {{{
//      *   // globals
//      *   // prelude
//      *   {
//      *     float y = 12.34;
//      *     bool yr = isDefined(y); // true
//      *     yr;
//      *   }
//      * }}}
//      */
//     "isDefined for Value return true" in {
//       val t = IGlobals.prelude ++ Block(
//         VarDecl(TypeRef(typeNames.f32Type), "y", FloatVal(12.34f)),
//         VarDecl(TypeRef(typeNames.boolType), "yr", Call(SymbolRef("isDefined"), List(Var(SymbolRef("y"))))),
//         Var(SymbolRef("yr"))
//       )

//       val errOrRes = eval(t)
//       errOrRes match
//         case Right(InterpretState(_, _, ms, c)) =>
//           c mustBe BoolCell(true)
//         case Left(t) => fail("Should be 'right", t)
//     }

//     /**
//      * {{{
//      *   // globals
//      *   // prelude
//      *   {
//      *     int x = null;
//      *     int y = 17;
//      *     int z = coalesce(x, y);
//      *     z; // 17
//      *   }
//      * }}}
//      */
//     "coalesce" in {
//       val t = IGlobals.prelude ++ Block(
//         VarDecl(TypeRef(typeNames.i32Type), "x", NothingVal()),
//         VarDecl(TypeRef(typeNames.i32Type), "y", IntVal(17)),
//         VarDecl(TypeRef(typeNames.i32Type), "z", Call(SymbolRef("coalesce"), List(Var(SymbolRef("x")), Var(SymbolRef("y"))))),
//         Var(SymbolRef("z"))
//       )

//       val errOrRes = eval(t)
//       errOrRes match
//         case Right(InterpretState(_, _, ms, c)) => c mustBe (IntCell(17))
//         case Left(t)                            => fail("Should be 'right", t)
//     }

//     /**
//      * {{{
//      *   // globals
//      *   // prelude
//      *   {
//      *     date d = today();
//      *     d;
//      *   }
//      * }}}
//      */
//     "today" in {
//       val t = IGlobals.prelude ++ Block(
//         VarDecl(TypeRef(typeNames.dateType), "d", Call(SymbolRef("today"), List.empty[Expr])),
//         Var(SymbolRef("d"))
//       )

//       val errOrRes = eval(t)
//       errOrRes match
//         case Right(InterpretState(_, _, ms, c)) => c.isInstanceOf[DateCell] mustBe true
//         case Left(t)                            => fail("Should be 'right", t)
//     }

//     /**
//      * {{{
//      *   // globals
//      *   // prelude
//      *   {
//      *     datetime d = now();
//      *     d;
//      *   }
//      * }}}
//      */
//     "now" in {
//       val t = IGlobals.prelude ++ Block(
//         VarDecl(TypeRef(typeNames.datetimeType), "d", Call(SymbolRef("now"), List.empty[Expr])),
//         Var(SymbolRef("d"))
//       )

//       val errOrRes = eval(t)
//       errOrRes match
//         case Right(InterpretState(_, _, ms, c)) => c.isInstanceOf[DateTimeCell] mustBe true
//         case Left(t)                            => fail("Should be 'right", t)
//     }
//   }

//   private def eval(ast0: AST): Either[Throwable, InterpretState] =
//     val types         = Types.make(typeNames)
//     val typeCheckLaws = ITypeCheckLaws.make(types)

//     Builder
//       .build(ast0, types, typeCheckLaws)
//       .flatMap({ astMeta =>
//         val ms = MemorySpace("globals")

//         val laws = IInterpretLaws.make(types, astMeta.meta)

//         val interpretVisitor = InterpretVisitor.make(laws)
//         val interpretState   = InterpretState.make(meta = astMeta.meta, Stash.empty, memSpace = ms, retValue = VoidCell)

//         astMeta.ast.visit(interpretState, interpretVisitor)
//       })

// object InterpretVisitorSpec {}
