package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type

/**
 * Access a field of a Struct
 *
 * To access members of a struct like a.x, we trigger two ref operations. The first ref operation looks up a to figure out what type it is. The second ref then resolves x within
 * a’s scope (upon seeing the . node). We match (possibly nested) member access expressions, e.g.:
 *
 * {{{
 *     "a", "a.b", "a.b.c", and so on
 * }}}
 *
 * Given «expr».x, member need’s «expr»’s type because it must look up x with the scope of «expr».
 */
final case class Access(a: LValue, b: LValue) extends LValue


object Access:
  private val sep: String = "."

/*
  def path: String =
    def iterate(value: LValue): String = value match
      case x: Var    => x.symbol.name
      case x: Access => List(iterate(x.a), iterate(x.b)).mkString(Access.sep)
      case x         => sys.error(s"Unsupported Type to get a Path: '${x}'")
    iterate(this)
*/