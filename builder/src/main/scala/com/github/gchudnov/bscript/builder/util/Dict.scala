package com.github.gchudnov.bscript.builder.util

import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary on Keys and Values they are attached to. A key can have many values, but one value is bound to one key only.
 *
 * @param keyValues
 *   maps a key to a collection of values
 * @param valueKey
 *   maps an value to the related key
 */
abstract class Dict[K, V, D <: Dict[K, V, D]]:

  protected def keyValues: Map[K, Set[V]]
  protected def valueKey: Map[V, K]

  protected def clone(keyValues: Map[K, Set[V]], valueKey: Map[V, K]): D

  def set(key: K, value: V): D =
    val vs = keyValues.getOrElse(key, Set.empty[V])

    require(!vs.contains(value), s"value ${value} is already assigned to the key ${key}, cannot set it twice.")
    require(!valueKey.contains(value), s"value ${value} is already assigned to a different key, cannot set it to the key ${key}.")

    val keyValues1 = keyValues + (key  -> (vs + value))
    val valueKey1  = valueKey + (value -> key)

    clone(
      keyValues = keyValues1,
      valueKey = valueKey1
    )

  protected def values(key: K): List[V] =
    keyValues.getOrElse(key, Set.empty[V]).toList

  protected def key(value: V): Option[K] =
    valueKey.get(value)
