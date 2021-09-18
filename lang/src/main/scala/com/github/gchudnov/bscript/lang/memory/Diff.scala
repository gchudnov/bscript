package com.github.gchudnov.bscript.lang.memory

object Diff:

  sealed trait Change[+K, +V]:
    def key: K
  case class Removed[K](key: K)                         extends Change[K, Nothing]
  case class Added[K, V](key: K, value: V)              extends Change[K, V]
  case class Updated[K, V](key: K, before: V, after: V) extends Change[K, V]

  def calc[K, V](before: Map[K, V], after: Map[K, V]): Iterable[Change[K, V]] =
    val b = Iterable.newBuilder[Change[K, V]]
    before.foreach { case (k, vb) =>
      after.get(k) match
        case Some(va) if vb != va => b += Updated(k, vb, va)
        case None                 => b += Removed(k)
        case _                    =>
    }
    after.foreach { case (k, va) =>
      if !before.contains(k) then b += Added(k, va)
    }
    b.result()
