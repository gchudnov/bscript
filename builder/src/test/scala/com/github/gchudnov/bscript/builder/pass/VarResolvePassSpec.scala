package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

import scala.util.control.Exception.*

/**
 * Var Resolve Pass Tests
 */
final class VarResolvePassSpec extends TestSpec:
  import VarResolvePassSpec.*

  "VarResolvePass" when {

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
            succeed
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
            succeed
          case Left(t) =>
            fail("Should be 'right", t)
      }

     /**
       * {{{
       *   // globals
       *   int x = 0;
       *   x = 1;
       * }}}
       */
      "resolve a referenced variable if it is present" in {
        val t = Examples.varDefUse

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            succeed
          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }

      /**
        * {{{
        *   // globals
        *   int x = 0;
        *   y = 1;
        * }}}
        */
      "fail to resolve a referenced variable if it is not present" in {
        val t = Examples.varNotDefined

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("'y' is not found")
      }
    }

    "a struct is present" should {

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
       *     fn f() -> void = {
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
      "resolve references to the fields of the struct" in {
        val t = Examples.struct

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            succeed
          case Left(t) =>
            println(t)
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phase 1 only.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 build
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0
    val buildOut         = buildPass.run(buildIn)

    // #2 resolve
    val resolvePass = new VarResolvePass()
    val resolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      val scopeTree  = buildOut.scopeTree
      val scopeSymbols = buildOut.scopeSymbols
      val scopeAsts = buildOut.scopeAsts
      val ast = ast0
    val resolveOut = resolvePass.run(resolveIn)

    // return the actual state
    val actualState = toActualState(resolveOut)
    actualState
  }

object VarResolvePassSpec:
  final case class ActualState(
  )

  def toActualState(s: Unit): ActualState =
    ActualState(
    )
