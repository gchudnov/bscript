package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.pass.scopebuild.InState as BuildInState
import com.github.gchudnov.bscript.builder.pass.scopebuild.OutState as BuildOutState
import com.github.gchudnov.bscript.builder.pass.scopebuild.PassImpl as BuildPassImpl
import com.github.gchudnov.bscript.builder.pass.scoperesolve.InState as ResolveInState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.OutState as ResolveOutState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.PassImpl as ResolvePassImpl
import com.github.gchudnov.bscript.builder.pass.typecheck.InState as TypeCheckInState
import com.github.gchudnov.bscript.builder.pass.typecheck.OutState as TypeCheckOutState
import com.github.gchudnov.bscript.builder.pass.typecheck.PassImpl as TypeCheckPassImpl

import scala.util.control.Exception.*

sealed trait Builder:

  def build(ast0: AST): Either[Throwable, (AST, BuildState)] =
    val buildPass     = new BuildPassImpl()
    val resolvePass   = new ResolvePassImpl()
    val typeCheckPass = new TypeCheckPassImpl()

    for
      buildStateIn      <- nonFatalCatch.either(BuildInState.from(ast0))
      buildOutState     <- nonFatalCatch.either(buildPass.run(buildStateIn))
      resolveStateIn    <- nonFatalCatch.either(ResolveInState.from(buildOutState.ast))
      resolveOutState   <- nonFatalCatch.either(resolvePass.run(resolveStateIn))
      typeCheckStateIn  <- nonFatalCatch.either(TypeCheckInState.from(resolveOutState.ast))
      typeCheckStateOut <- nonFatalCatch.either(typeCheckPass.run(typeCheckStateIn))
      ast1  = typeCheckStateOut.ast 
      buildState         = BuildState.from(typeCheckStateOut.ast)
    yield (ast1, buildState)

object Builder extends Builder
