package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.TestSpec
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*

import scala.util.control.Exception.*
import com.github.gchudnov.bscript.builder.Examples
import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.lang.ast.types.BuiltInType
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.func.ASTFinder
import com.github.gchudnov.bscript.lang.ast.lit.ConstLit
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.refs.*

/**
 * Type Check Pass Tests
 */
final class TypecheckPassSpec extends TestSpec:
  import TypeCheckPassSpec.*

  "TypeCheckPass" when {

    "const literals" should {

      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "check the type" in {
        val t = Examples.intVal

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
   * To evaluate, we run Phases 1, 2 and 3.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve vars in AST-nodes.
   *   - In Phase 3 we resolve types of AST-nodes.
   *   - In Phase 4 we check types of AST-nodes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 scope build
    val buildPass = new ScopeBuildPass()
    val buildIn = new HasAST:
      val ast = ast0

    val buildOut = buildPass.run(buildIn)

    // #2 var resolve
    val varResolvePass = new VarResolvePass()
    val varResolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val _ = varResolvePass.run(varResolveIn)

    // #3 type resolve
    val typeResolvePass = new TypeResolvePass()
    val typeResolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val typeResolveOut = typeResolvePass.run(typeResolveIn)

    // #4 type check
    val typeCheckPass = new TypeCheckPass()
    val typeCheckIn = new HasEvalTypes with HasAST:
      override val evalTypes: EvalTypes = typeResolveOut.evalTypes
      override val ast: AST                   = ast0

    val typeCheckOut = typeCheckPass.run(typeCheckIn)

    // return the actual state
    val actualState = toActualState(typeCheckOut)
    actualState
  }

object TypeCheckPassSpec:
  final case class ActualState(
  )

  def toActualState(s: Unit): ActualState =
    ActualState(
    )
