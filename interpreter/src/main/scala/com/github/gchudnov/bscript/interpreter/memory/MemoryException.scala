package com.github.gchudnov.bscript.interpreter.memory

import com.github.gchudnov.bscript.lang.LangException

final class MemoryException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object MemoryException:
  def unapply(e: MemoryException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
