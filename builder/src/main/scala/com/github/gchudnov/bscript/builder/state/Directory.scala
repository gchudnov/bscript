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
abstract class Directory[K, V <: AnyRef, D <: Directory[K, V, D]]:

  val keyValues: Map[K, Set[Ptr[V]]]
  val valueKey: Map[Ptr[V], K]

  def clone(keyValues: Map[K, Set[Ptr[V]]], valueKey: Map[Ptr[V], K]): D

  def addKey(key: K): D =
    clone(keyValues = this.keyValues + (key -> Set.empty[Ptr[V]]), valueKey = valueKey)

  def link(key: K, value: V): D =
    val vs = keyValues.getOrElse(key, Set.empty[Ptr[V]])

    assert(!vs.contains(Ptr(value)), s"Value ${value} is already linked to the Key ${key}, cannot link it twice.")

    val keyValues1 = keyValues + (key       -> (vs + Ptr(value)))
    val valueKey1  = valueKey + (Ptr(value) -> key)

    clone(
      keyValues = keyValues1,
      valueKey = valueKey1
    )
