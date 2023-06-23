package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST

import scala.util.control.Exception.*

sealed trait Builder:

  def build(ast0: AST): Either[Throwable, (AST, BuildState)] =
    ???
    // val buildPass     = new BuildPassImpl()
    // val resolvePass   = new ResolvePassImpl()
    // val typeCheckPass = new TypeCheckPassImpl()

    // for
    //   buildStateIn      <- nonFatalCatch.either(BuildInState.from(ast0))
    //   buildOutState     <- nonFatalCatch.either(buildPass.run(buildStateIn))
    //   resolveStateIn    <- nonFatalCatch.either(ResolveInState.from(buildOutState.ast, buildOutState.scopeTree, buildOutState.scopeSymbols, buildOutState.scopeAsts))
    //   resolveOutState   <- nonFatalCatch.either(resolvePass.run(resolveStateIn))
    //   typeCheckStateIn  <- nonFatalCatch.either(TypeCheckInState.from(resolveOutState.ast, resolveOutState.scopeAsts))
    //   typeCheckStateOut <- nonFatalCatch.either(typeCheckPass.run(typeCheckStateIn))
    //   ast1  = typeCheckStateOut.ast 
    //   buildState         = BuildState.from(typeCheckStateOut.ast)
    // yield (ast1, buildState)

object Builder extends Builder
