package com.github.gchudnov.bscript.lang.util

/**
 * Wrapper to allow the same case class to be put twice in a Map
 * {{{
 *   case class A(x: Int)
 *
 *   val m0 = Map.empty[EqWrap[A], String]
 *   val m1 = m0 + (A(1) -> "10")
 *   val m2 = m1 + (A(1) -> "20")
 *
 *   // at the end there should be several elements in Map, since we have two separate A instances
 * }}}
 */
class EqWrap[T <: AnyRef](val value: T):
  override def hashCode(): Int =
    if value == null then 0 else value.hashCode

  override def equals(a: Any): Boolean = a match
    case ref: EqWrap[?] => ref.value eq value
    case _              => false

  override def toString: String =
    s"EqWrap(${value.toString})"
object EqWrap:
  def apply[T <: AnyRef](t: T) = new EqWrap(t)
