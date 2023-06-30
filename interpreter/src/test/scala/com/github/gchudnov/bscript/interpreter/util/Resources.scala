package com.github.gchudnov.bscript.interpreter.util

import scala.io.Source
import scala.util.Using


object Resources:

  def asString(resPath: String): Either[Throwable, String] =
    Using(Source.fromResource(resPath)) { source =>
      source.getLines().mkString(sys.props("line.separator"))
    }.toEither
