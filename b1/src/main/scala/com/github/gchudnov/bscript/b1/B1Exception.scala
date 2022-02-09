package com.github.gchudnov.bscript.b1

import com.github.gchudnov.bscript.lang.LangException

final class B1Exception(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object B1Exception:
  def unapply(e: B1Exception): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
