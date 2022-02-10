package com.github.gchudnov.bscript.b1.internal.util

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Using
import scala.util.control.Exception.allCatch

private[b1] object FileOps:

  def stringFromPath(path: Path): Either[Throwable, String] =
    allCatch.either(Files.readString(path, StandardCharsets.UTF_8))

  def saveString(path: Path, data: String): Either[Throwable, Unit] =
    allCatch.either {
      Using.resource(new BufferedWriter(new FileWriter(path.toFile))) { writer =>
        writer.write(data)
      }
    }
