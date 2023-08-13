package com.github.gchudnov.bscript.lang.ast.types

/**
 * ByName AST node.
 *
 * Equialent to Scala's `=>`: `def f(x: => Int) = x + x`
 *
 * Used for lazy evaluation of arguments.
 */
final case class ByName(aType: TypeAST) extends TypeAST
