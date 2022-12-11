package com.github.gchudnov.bscript.builder.util

/**
 * Wrapper to distinguish case classes by instance, e.g. to allow the case classes be be keys of a Map
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
class Ptr[T <: AnyRef](val value: T):
  override def hashCode(): Int =
    if value == null then 0 else value.hashCode

  override def equals(a: Any): Boolean = a match
    case ref: Ptr[?] => ref.value eq value
    case _              => false

  override def toString: String =
    s"EqWrap(${value.toString})"

object Ptr:
  def apply[T <: AnyRef](t: T) = new Ptr(t)
