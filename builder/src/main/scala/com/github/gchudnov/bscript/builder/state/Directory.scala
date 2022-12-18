package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary on Keys and Values they are attached to. A key can have many values, but one value is bound to one key only.
 *
 * @param keyValues
 *   maps a key to a collection of values
 * @param valueKey
 *   maps an value to the related key
 */
case class Directory[K, V <: AnyRef](
  keyValues: Map[K, Set[Ptr[V]]],
  valueKey: Map[Ptr[V], K]
):

  def addKey(key: K): Directory[K, V] =
    this.copy(keyValues = this.keyValues + (key -> Set.empty[Ptr[V]]))

  def link(key: K, value: V): Directory[K, V] =
    val vs = keyValues.getOrElse(key, Set.empty[Ptr[V]])

    assert(!vs.contains(Ptr(value)), s"Value ${value} is already linked to the Key ${key}, cannot link it twice.")

    val keyValues1 = keyValues + (key       -> (vs + Ptr(value)))
    val valueKey1  = valueKey + (Ptr(value) -> key)

    this.copy(
      keyValues = keyValues1,
      valueKey = valueKey1
    )

object Directory:
  def empty[K, V <: AnyRef]: Directory[K, V] =
    Directory(
      keyValues = Map.empty[K, Set[Ptr[V]]],
      valueKey = Map.empty[Ptr[V], K]
    )
