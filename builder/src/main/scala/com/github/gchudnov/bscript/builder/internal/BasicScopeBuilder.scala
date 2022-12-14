package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.util.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.util.ForestCursor


/**
 * BasicScopeBuilder
 */
final class BasicScopeBuilder(cursor: ForestCursor[Scope]) extends ScopeBuilder:
  import Meta.*

  override def push(): Unit =
    ???    

  override def pop(): Unit =
    ???

  override def define(s: Symbol): Unit =
    ???

  override def result: Meta =
    // meta
    ???

