package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.state.ForestCursor


/**
 * BasicScopeBuilder
 */
final class BasicScopeBuilder(cursor: ForestCursor[Scope]) extends ScopeBuilder:
  import Meta.*

  override def push(): ScopeBuilder =
    new BasicScopeBuilder(cursor.push())

  override def pop(): ScopeBuilder =
    new BasicScopeBuilder(cursor.pop())

  override def define(s: Symbol): ScopeBuilder =
    cursor.current match {
      case Some(scope) =>
        ???
      case None =>
        throw new BuilderException(s"Cannot define '${s}' symbol without any scope. Run .push() first to create a scope")
    }

  override def result: Meta =
    Meta.empty
