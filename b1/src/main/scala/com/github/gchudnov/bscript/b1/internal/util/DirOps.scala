package com.github.gchudnov.bscript.b1.internal.util

import java.nio.file.Files

import scala.util.control.Exception.allCatch
import java.nio.file.Path

object DirOps:

  def createTempDir(prefix: String): Either[Throwable, Path] =
    allCatch.either(Files.createTempDirectory(prefix))
