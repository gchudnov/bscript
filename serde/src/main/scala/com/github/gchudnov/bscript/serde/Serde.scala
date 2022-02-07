package com.github.gchudnov.bscript.serde

import scala.util.control.Exception.allCatch

trait Serde[R <: Throwable, T] extends Deserializer[R, T] with Serializer[R, T]:

  def inmap[U](f: T => U)(g: U => T): Serde[R, U] =
    Serde(map(f))(contramap(g))

  def inmapM[R1 >: R <: Throwable, U](f: T => Either[R1, U])(g: U => Either[R1, T]): Serde[R1, U] =
    Serde(flatMap(f))(contramapM(g))

object Serde:

  def apply[R <: Throwable, T](des: (String) => Either[R, T])(ser: (T) => Either[R, String]): Serde[R, T] =
    new Serde[R, T]:
      override def serialize(value: T): Either[R, String] =
        ser(value)
      override def deserialize(data: String): Either[R, T] =
        des(data)

  def apply[R <: Throwable, T](des: Deserializer[R, T])(ser: Serializer[R, T]): Serde[R, T] =
    new Serde[R, T]:
      override def serialize(value: T): Either[R, String] =
        ser.serialize(value)
      override def deserialize(data: String): Either[R, T] =
        des.deserialize(data)

  def from[T](des: (String) => T)(ser: (T) => String): Serde[Throwable, T] =
    new Serde[Throwable, T]:
      override def serialize(value: T): Either[Throwable, String] =
        allCatch.either(ser(value))
      override def deserialize(data: String): Either[Throwable, T] =
        allCatch.either(des(data))
