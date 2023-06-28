package com.github.gchudnov.bscript.builder.util

/**
 * A bi-directional dictionary on Keys and Values they are attached to.
 * 
 * A key can have many values, but one value is bound to one key only.
 *
 * @param keyValues
 *   maps a key to a collection of values
 * @param valueKey
 *   maps an value to the related key
 */
abstract class BiDict[K: Show, V: Show, D <: BiDict[K, V, D]]:

  protected def keyValues: Map[K, Set[V]]
  protected def valueKey: Map[V, K]

  protected def clone(keyValues: Map[K, Set[V]], valueKey: Map[V, K]): D

  /**
   * Set a value for a key.
   *
   * @param key
   *   key
   * @param value
   *   value
   * @return
   *   a new dictionary with the value set for the key
   */
  def set(key: K, value: V): D =
    val vs = keyValues.getOrElse(key, Set.empty[V])

    require(!vs.contains(value), s"value ${value} is already assigned to the key ${key}, most likely we do not want to set it twice. Check for bugs in the code.")

    val keyValues1 = keyValues + (key  -> (vs + value))
    val valueKey1  = valueKey + (value -> key)

    clone(
      keyValues = keyValues1,
      valueKey = valueKey1,
    )

  /**
   * Checks whether the value is assigned to the key.
   *
   * @param key
   *   key
   * @param value
   *   value
   * @return
   *   true if the value is assigned to the key, false otherwise
   */
  def contains(key: K, value: V): Boolean =
    keyValues.get(key).exists(_.contains(value))

  /**
   * Get values for the given key.
   */
  protected def values(key: K): List[V] =
    keyValues.getOrElse(key, Set.empty[V]).toList

  /**
   * Get a key for the given value.
   * @param value
   *   value
   * @return
   *   a key if the value is assigned to a key, None otherwise
   */
  protected def key(value: V): Option[K] =
    valueKey.get(value)

  /**
   * Represent as a string
   */
  def show: String =
    val showK = summon[Show[K]]
    val showV = summon[Show[V]]

    val kvs = keyValues.toList.map { case (k, vs) => (showK.show(k), vs.toList.map(v => showV.show(v)).sorted) }
      .sortBy(_._1)
      .map { case (k, vs) => s"\"${k}\": ${Strings.arrayAsString(vs.map(v => Strings.quoted(v)))}" }

    val sb = new StringBuilder()
    sb.append("{\n")
    kvs.foreach(kv => sb.append(Strings.spaced(1)).append(kv).append("\n"))
    sb.append("}\n")

    sb.toString()
