package com.github.gchudnov.bscript.lang.symbols

/**
 * Symbol - a generic programming language symbol - contains information about a language element.
 *
 * Symbols have:
 *
 * 1) Name - Symbols are usually identifiers like x, f, and T, but they can be operators too. For example, in Ruby, we can use operators like + as method names.
 *
 * 2) Category - what kind of thing the symbol is. Is it a class, method, variable, label, and so on.
 *
 * 3) Type - To validate operation x+y, for example, we need to know the types of x and y. Dynamically typed languages like Python track type information at run-time. Statically
 * typed languages like C++ and Java track type information at compile time.
 */
trait Symbol extends Named

object Symbol:
  val Undefined: Symbol = new Symbol:
    override val name: String     = "symbol:UNDEFINED"
    override def toString: String = s"${name}"
