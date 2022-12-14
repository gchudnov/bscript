package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.builder.util.Forest
import com.github.gchudnov.bscript.builder.Meta

/**
 * Scope
 *
 * A scope is a code region with a well-defined boundary that groups symbol definitions (in a dictionary associated with that code region).
 *
 * Scope boundaries usually coincide with begin and end tokens such as curly braces. We call this *lexical scoping* because the extent of a scope is lexically delimited.
 *
 * Here’s a list of scope characteristics:
 *
 * 1) Static vs. dynamic scoping: Most languages have static scoping, but some (like classic LISP and PostScript) have dynamic scoping. Think of dynamic scoping as allowing methods
 * to see the local variables of invoking methods.
 *
 * 2) Named scopes: Many scopes, like classes and methods have names, but global and local scopes don’t.
 *
 * 3) Nesting: Languages usually allow some form of scope nesting.
 *
 * 4) Contents: Some scopes allow declarations, some allow statements, and some allow both. C structs only allow declarations.
 *
 * 5) Visibility: The symbols in a scope might or might not be visible to some other code section.
 *
 * Scopes don’t need to track the code region from which we create them. Instead, the AST for the code regions point to their scopes. This makes sense because we’re going to look
 * up symbols in scopes according to what we find in the AST nodes.
 *
 * A reference’s scope stack is the set of scopes on the path to the root of the scope tree. We call this stack the *semantic context*.
 *
 * So, to resolve a symbol reference, we look for it in its semantic context, starting with the current scope. If resolve() doesn’t find the symbol in the current scope, it asks
 * the enclosing scope if it can find the symbol. resolve() recursively walks toward the root of the scope tree until it finds the symbol or runs out of scopes.
 */

/**
 * ScopeBuilder
 */
trait ScopeBuilder:
  def push(): Unit
  def pop(): Unit

  def define(s: Symbol): Unit

  def result: Meta

/**
 * ScopeBuilder
 */
object ScopeBuilder:

  def make(meta: Meta): ScopeBuilder =
    new BasicScopeBuilder(meta)
