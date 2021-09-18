package com.github.gchudnov.bscript.lang.ast.serde

trait Serializer[R <: Throwable, -T]:
  def serialize(value: T): Either[R, String]

  def contramap[U](f: U => T): Serializer[R, U] =
    Serializer((value) => serialize(f(value)))

  def contramapM[R1 >: R <: Throwable, U](f: U => Either[R1, T]): Serializer[R1, U] =
    Serializer((value) => f(value).flatMap(serialize))

  def asOption[U <: T](implicit ev: Null <:< T): Serializer[R, Option[U]] =
    contramap(_.orNull)

object Serializer:
  def apply[R <: Throwable, T](ser: (T) => Either[R, String]): Serializer[R, T] =
    (value: T) => ser(value)
