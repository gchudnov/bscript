package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.LangException

final class CompiledException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object CompiledException:
  def unapply(e: CompiledException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
