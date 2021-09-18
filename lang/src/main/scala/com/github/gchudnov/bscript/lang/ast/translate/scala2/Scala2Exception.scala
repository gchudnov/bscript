package com.github.gchudnov.bscript.lang.ast.translate.scala2

import com.github.gchudnov.bscript.lang.LangException

final class Scala2Exception(message: String) extends LangException(message):

  def this(message: String, cause: Throwable) =
    this(message)
    initCause(cause)

  def this(cause: Throwable) =
    this(Option(cause).map(_.toString).orNull, cause)

  def this() =
    this(null: String)

object Scala2Exception:
  def unapply(e: Scala2Exception): Option[(String, Throwable)] = Some((e.getMessage, e.getCause))
