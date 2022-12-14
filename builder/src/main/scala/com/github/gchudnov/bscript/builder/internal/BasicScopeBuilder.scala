package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.util.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta


/**
 * BasicScopeBuilder
 */
final class BasicScopeBuilder(meta: Meta) extends ScopeBuilder:
  import Meta.*

  private var scopeTree: ScopeTree = Forest.empty[Scope]
  private var parent: Option[Scope] = None
  
  private var counter: Vector[Int] = Vector.empty[Int]
  private var level: Int = -1

  override def push(): Unit =
    ???    

  override def pop(): Unit =
    ???

  override def define(s: Symbol): Unit =
    ???

  override def result: Meta =
    meta
