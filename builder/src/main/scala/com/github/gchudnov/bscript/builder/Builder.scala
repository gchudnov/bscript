package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.pass.*
import com.github.gchudnov.bscript.builder.env.*

import scala.util.control.Exception.*

object Builder:

  def build(ast0: AST): Either[Throwable, (AST, BuildState)] =
    val buildPass       = new ScopeBuildPass()
    val symResolvePass  = new SymbolResolvePass()
    val typeResolvePass = new TypeResolvePass()
    val typeCheckPass   = new TypeCheckPass()

    for
      buildIn        <- nonFatalCatch.either(toBuildIn(ast0))
      buildOut       <- nonFatalCatch.either(buildPass.run(buildIn))
      symResolveIn   <- nonFatalCatch.either(toSymResolveIn(buildOut, ast0))
      symResolveOut  <- nonFatalCatch.either(symResolvePass.run(symResolveIn)) // NOTE: at the moment we ignore result of this pass
      typeResolveIn  <- nonFatalCatch.either(toTypeResolveIn(buildOut, ast0))
      typeResolveOut <- nonFatalCatch.either(typeResolvePass.run(typeResolveIn))
      typeCheckIn    <- nonFatalCatch.either(toTypeCheckIn(typeResolveOut, ast0))
      typeCheckOut   <- nonFatalCatch.either(typeCheckPass.run(typeCheckIn)) // NOTE: at the moment we ignore result of this pass
    yield (ast0, BuildState.from(ast0))

  /**
   * -> Build In
   */
  private def toBuildIn(ast: AST): HasAST =
    HasAST(ast)

  /**
   * Build Out -> Symbol Resolve In
   */
  private def toSymResolveIn(s: HasScopeTree & HasScopeSymbols & HasScopeAsts, ast0: AST): HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST =
    new HasReadScopeTree with HasReadScopeSymbols with HasReadScopeAsts with HasAST:
      override val scopeTree: ReadScopeTree       = s.scopeTree
      override val scopeSymbols: ReadScopeSymbols = s.scopeSymbols
      override val scopeAsts: ReadScopeAsts       = s.scopeAsts
      override val ast: AST                       = ast0

  /**
   * Build Out -> Type Resolve In
   */
  private def toTypeResolveIn(s: HasScopeTree & HasScopeSymbols & HasScopeAsts, ast0: AST): HasReadScopeTree & HasReadScopeSymbols & HasReadScopeAsts & HasAST =
    new HasReadScopeTree with HasReadScopeSymbols with HasReadScopeAsts with HasAST:
      override val scopeTree: ScopeTree       = s.scopeTree
      override val scopeSymbols: ScopeSymbols = s.scopeSymbols
      override val scopeAsts: ScopeAsts       = s.scopeAsts
      override val ast: AST                   = ast0

  /**
   * Type Resolve Out -> Type Check In
   */
  private def toTypeCheckIn(s: HasEvalTypes, ast0: AST): HasReadEvalTypes & HasAST =
    new HasReadEvalTypes with HasAST:
      override val evalTypes: ReadEvalTypes = s.evalTypes
      override val ast: AST                 = ast0
