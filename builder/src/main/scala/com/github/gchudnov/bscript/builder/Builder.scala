package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.pass.*
import com.github.gchudnov.bscript.builder.env.*

import scala.util.control.Exception.*

object Builder:

  def build(ast0: AST): Either[Throwable, (AST, BuildState)] =
    val buildPass       = new ScopeBuildPass()
    val varResolvePass  = new VarResolvePass()
    val typeResolvePass = new TypeResolvePass()

    for
      buildIn        <- nonFatalCatch.either(toBuildIn(ast0))
      buildOut       <- nonFatalCatch.either(buildPass.run(buildIn))
      varResolveIn   <- nonFatalCatch.either(buildOutToVarResolveIn(buildOut, ast0))
      _              <- nonFatalCatch.either(varResolvePass.run(varResolveIn)) // NOTE: at the moment we ignore result of this pass
      typeResolveIn  <- nonFatalCatch.either(buildOutToTypeResolveIn(buildOut, ast0))
      typeResolveOut <- nonFatalCatch.either(typeResolvePass.run(typeResolveIn))
    yield (ast0, BuildState.from(ast0))

  /**
   * -> Build In
   */
  private def toBuildIn(ast: AST): HasAST =
    HasAST(ast)

  /**
   * Build Out -> Var Resolve In
   */
  private def buildOutToVarResolveIn(s: HasScopeTree & HasScopeSymbols & HasScopeAsts, ast0: AST): HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST =
    new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      val scopeTree    = s.scopeTree
      val scopeSymbols = s.scopeSymbols
      val scopeAsts    = s.scopeAsts
      val ast          = ast0

  /**
   * Build Out -> Resolve In
   */
  private def buildOutToTypeResolveIn(s: HasScopeTree & HasScopeSymbols & HasScopeAsts, ast0: AST): HasScopeTree & HasScopeSymbols & HasScopeAsts & HasAST =
    new HasScopeTree with HasScopeSymbols with HasScopeAsts with HasAST:
      val scopeTree    = s.scopeTree
      val scopeSymbols = s.scopeSymbols
      val scopeAsts    = s.scopeAsts
      val ast          = ast0
