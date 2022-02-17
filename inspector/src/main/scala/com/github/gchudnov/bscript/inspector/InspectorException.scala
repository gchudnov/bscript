package com.github.gchudnov.bscript.inspector

import com.github.gchudnov.bscript.lang.LangException

final class InspectorException(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object InspectorException:
  def unapply(e: InspectorException): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
