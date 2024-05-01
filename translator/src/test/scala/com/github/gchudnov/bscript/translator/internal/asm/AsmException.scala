package com.github.gchudnov.bscript.translator.internal.asm

import com.github.gchudnov.bscript.lang.LangException

final class AsmException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object AsmException:
  def unapply(e: AsmException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
