package com.github.gchudnov.bscript.lang.util
import scala.collection.Factory
import scala.collection.immutable.SeqOps

object Transform:

  def sequence[E, A](es: Seq[Either[E, A]]): Either[E, Seq[A]] = es.partitionMap(identity) match
    case (Nil, rights) => Right(rights)
    case (lefts, _)    => Left(lefts.head)

  def sequence[E, A](es: Option[Either[E, A]]): Either[E, Option[A]] = es match
    case None => Right(None)
    case Some(e) =>
      e match
        case Left(x)  => Left(x)
        case Right(x) => Right(Some(x))

  def traverse[A, CC[A] <: collection.Iterable[A], E, B](xs: CC[A])(f: A => Either[E, B])(implicit cbf: Factory[B, CC[B]]): Either[E, CC[B]] =
    val builder = cbf.newBuilder
    val i       = xs.iterator
    while i.hasNext do
      f(i.next()) match
        case Right(b) => builder += b
        case Left(e)  => return Left(e)
    Right(builder.result())
