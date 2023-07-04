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

/**
 * Type Resolve Pass Tests
 */
final class TypeResolvePassSpec extends TestSpec:
  import TypeResolvePassSpec.*
  import TypeAST.given

  val showTypeAST = summon[Show[TypeAST]]

  "TypeResolvePass" when {

    "const literals" should {

      /**
       * {{{
       *   // globals
       *   2;
       * }}}
       */
      "build scope for an integer" in {
        val t = Examples.intVal

        val constFinder = new ASTFinder:
          override def findAST(ast: AST): Option[AST] =
            ast match
              case _: ConstLit => Some(ast)
              case _           => None

        val errOrRes = eval(t.ast)
        errOrRes match
          case Right(actualState) =>
            actualState.evalTypes.isEmpty mustBe false

            val node = constFinder.foldAST(None, t.ast)
            actualState.evalTypes(node.get) mustBe BuiltInType(TypeName.i32)

          case Left(t) =>
            fail("Should be 'right", t)
      }
    }
  }

  /**
   * To evaluate, we run Phases 1, 2.
   *
   *   - In Phase 1 we build scopes and define symbols in scopes.
   *   - In Phase 2 we resolve types of AST-nodes.
   */
  private def eval(ast0: AST): Either[Throwable, ActualState] = nonFatalCatch.either {
    // #1 build
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

    // #3 resolve
    val typeResolvePass = new TypeResolvePass()
    val typeResolveIn = new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = buildOut.scopeTree
      override val scopeSymbols: ScopeSymbols = buildOut.scopeSymbols
      override val scopeAsts: ScopeAsts       = buildOut.scopeAsts
      override val ast: AST                   = ast0

    val resolveOut = typeResolvePass.run(typeResolveIn)

    // return the actual state
    val actualState = toActualState(resolveOut)
    actualState
  }

object TypeResolvePassSpec:
  final case class ActualState(
    evalTypes: EvalTypes,
  )

  def toActualState(s: HasEvalTypes): ActualState =
    ActualState(
      evalTypes = s.evalTypes,
    )
