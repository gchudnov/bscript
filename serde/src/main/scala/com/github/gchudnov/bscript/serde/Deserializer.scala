package com.github.gchudnov.bscript.serde

import scala.annotation.nowarn
import scala.util.{Failure, Success, Try}

trait Deserializer[R <: Throwable, +T]:
  def deserialize(data: String): Either[R, T]

  def map[U](f: T => U): Deserializer[R, U] = Deserializer(deserialize(_).map(f))

  def flatMap[R1 >: R <: Throwable, U](f: T => Either[R1, U]): Deserializer[R1, U] = Deserializer(deserialize(_).flatMap(f))

  def orElse[R1 >: R <: Throwable, U >: T](alt: Deserializer[R1, U]): Deserializer[R1, U] =
    Deserializer(data => deserialize(data).orElse(alt.deserialize(data)))

  def asTry: Deserializer[R, Try[T]] =
    Deserializer(deserialize(_).fold(e => Right(Failure(e)), v => Right(Success(v))))

  def asOption(implicit @nowarn ev: T <:< AnyRef): Deserializer[R, Option[T]] =
    Deserializer(deserialize(_).fold(_ => Right(None), v => Right(Some(v))))

object Deserializer:
  def apply[R <: Throwable, T](des: (String) => Either[R, T]): Deserializer[R, T] =
    (data: String) => des(data)
