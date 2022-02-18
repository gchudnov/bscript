package com.github.gchudnov.bscript.interpreter.memory

object Diff:

  sealed trait Change[+K, +V]:
    def key: K
  case class Removed[K, V](key: K, value: V)            extends Change[K, V]
  case class Added[K, V](key: K, value: V)              extends Change[K, V]
  case class Updated[K, V](key: K, before: V, after: V) extends Change[K, V]

  def calc[K, V](before: Map[K, V], after: Map[K, V]): Iterable[Change[K, V]] =
    val rs = Iterable.newBuilder[Change[K, V]]

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

  def calc[K, V](key: K, before: Seq[V], after: Seq[V]): Iterable[Change[K, V]] =
    val rs = Iterable.newBuilder[Change[K, V]]

    after.zip(before).foreach { case (va, vb) =>
      if vb != va then rs += Updated(key, vb, va)
    }

    if before.size > after.size then
      val bTail = before.drop(after.size)
      bTail.foreach(vb => rs += Removed(key, vb))
    else if after.size > before.size then
      val aTail = after.drop(before.size)
      aTail.foreach(va => rs += Added(key, va))

    rs.result()
