package com.github.gchudnov.bscript.interpreter

import com.github.gchudnov.bscript.lang.LangException

final class InterpreterException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object InterpreterException:
  def unapply(e: InterpreterException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
