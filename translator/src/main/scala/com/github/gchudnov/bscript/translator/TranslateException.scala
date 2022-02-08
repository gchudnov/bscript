package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.LangException

final class TranslateException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object TranslateException:
  def unapply(e: TranslateException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
