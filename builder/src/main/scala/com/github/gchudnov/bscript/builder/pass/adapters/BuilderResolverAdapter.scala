package com.github.gchudnov.bscript.builder.pass.adapters

import com.github.gchudnov.bscript.builder.pass.scopebuilder.ScopeBuildOutState
import com.github.gchudnov.bscript.builder.pass.scoperesolver.ScopeResolveInState

final class BuilderResolverAdapter() extends Adapter[ScopeBuildOutState, ScopeResolveInState]:

  override def map(in: ScopeBuildOutState): ScopeResolveInState =
    ???
