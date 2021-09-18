package com.github.gchudnov.bscript.lang.symbols

import com.github.gchudnov.bscript.lang.LangException

final class ScopeStateException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)
