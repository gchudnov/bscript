package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

import scala.util.control.Exception.*
import com.github.gchudnov.bscript.builder.Examples

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

    "var is undefined" should {

      /**
       * {{{
       *   // globals
       *   x;
       * }}}
       */
      "fail to reference the indefined variable" in {
        val t = Examples.xRefUndefined

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("'x' is not found")
      }

      /**
       * {{{
       *   // globals
       *   int x = 0;
       *   y = 1;
       * }}}
       */
      "fail to resolve a referenced variable in assignment" in {
        val t = Examples.varNotDefined

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(_) =>
            fail("Should be 'left")
          case Left(t) =>
            t.getMessage must include("'y' is not found")
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
        val t = Examples.xDeclAssign

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            succeed
          case Left(t) =>
            fail("Should be 'right", t)
      }
    }

    "a struct is defined" should {

      /**
       * A small structure reference
       *
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
      "resolve references to a struct" in {
        val t = Examples.structA

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            succeed
          case Left(t) =>
            fail("Should be 'right", t)
      }

      /**
       * A small structure reference and assignment
       *
       * {{{
        *   // globals
        *   {
        *     struct A { int x; };
        *
        *     A a;
        *     a.x = 1;
        *     a;
        *   }
       * }}}
       */
      "resolve access to a field in a struct" in {
        val t = Examples.structAFieldAssignment

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
      "resolve references to fields" in {
        val t = Examples.struct

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            succeed
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
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 build
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0
    val buildOut = buildPass.run(buildIn)

    // #2 resolve
    val resolvePass = new VarResolvePass()
    val resolveIn = new HasReadScopeTree with HasReadScopeSymbols with HasScopeAsts with HasAST:
      val scopeTree    = buildOut.scopeTree
      val scopeSymbols = buildOut.scopeSymbols
      val scopeAsts    = buildOut.scopeAsts
      val ast          = ast0
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
