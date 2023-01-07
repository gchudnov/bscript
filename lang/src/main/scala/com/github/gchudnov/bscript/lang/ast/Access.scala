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
final case class Access(a: Ref, b: Id) extends Ref

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

/*

      ** Select a term member by symbol *
      def apply(qualifier: Term, symbol: Symbol): Select

      ** Select a field or a non-overloaded method by name
 *
 *  @note The method will produce an assertion error if the selected
 *        method is overloaded. The method `overloaded` should be used
 *        in that case.
 *
      def unique(qualifier: Term, name: String): Select

      ** Call an overloaded method with the given type and term parameters *
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term

      ** Call an overloaded method with the given type and term parameters *
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term


 */
