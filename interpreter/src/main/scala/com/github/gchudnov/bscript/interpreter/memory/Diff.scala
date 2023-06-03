package com.github.gchudnov.bscript.interpreter.memory

object Diff:

  sealed trait Change[+K, +V]:
    def key: K
  case class Removed[K, V](key: K, value: V)            extends Change[K, V]
  case class Added[K, V](key: K, value: V)              extends Change[K, V]
  case class Updated[K, V](key: K, before: V, after: V) extends Change[K, V]

  /**
    * Calculates the difference between two maps.
    *
    * @param before The map before the change.
    * @param after The map after the change.
    * @return The list of changes.
    */
  def calc[K, V](before: Map[K, V], after: Map[K, V]): List[Change[K, V]] =
    val rs = List.newBuilder[Change[K, V]]

    before.foreach { case (k, vb) =>
      after.get(k) match
        case Some(va) if vb != va => rs += Updated(k, vb, va)
        case None                 => rs += Removed(k, vb)
        case _                    =>
    }

    after.foreach { case (k, va) =>
      if !before.contains(k) then rs += Added(k, va)
    }

    rs.result()

  /**
    * Calculates the difference between two lists.
    *
    * @param before The list before the change.
    * @param after The list after the change.
    * @return The list of changes.
    */
  def calc[V](before: Iterable[V], after: Iterable[V]): List[Change[Int, V]] =
    val rs = List.newBuilder[Change[Int, V]]

    (before.zip(after)).zipWithIndex.foreach { case ((vb, va), i) =>
      if vb != va then rs += Updated(i, vb, va)
    }

    if before.size > after.size then
      val bTail = before.drop(after.size)
      bTail.zipWithIndex.foreach { case (vb, i) => rs += Removed(i + after.size, vb) }
    else if after.size > before.size then
      val aTail = after.drop(before.size)
      aTail.zipWithIndex.foreach { case (va, i) => rs += Added(i + before.size, va) }

    rs.result()
