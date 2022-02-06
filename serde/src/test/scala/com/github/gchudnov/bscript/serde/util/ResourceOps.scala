package com.github.gchudnov.bscript.serde.util

import java.io.FileOutputStream
import java.nio.file.Path
import scala.io.Source
import scala.util.Using
import scala.util.control.Exception.allCatch

object ResourceOps:

  def resourceToString(resourcePath: String): Either[Throwable, String] =
    Using(Source.fromResource(resourcePath)) { source =>
      source.getLines().mkString(sys.props("line.separator"))
    }.toEither
