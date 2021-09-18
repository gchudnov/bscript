package com.github.gchudnov.bscript.lang

class LangException(message: String) extends RuntimeException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object LangException:
  def unapply(e: LangException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
