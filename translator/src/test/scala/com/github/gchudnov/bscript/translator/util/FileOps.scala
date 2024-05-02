package com.github.gchudnov.bscript.translator.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.util.control.Exception.allCatch

private[translator] object FileOps:

  def stringFrom(path: Path): Either[Throwable, String] =
    allCatch.either(Files.readString(path, StandardCharsets.UTF_8))

  def saveString(path: Path, data: String): Either[Throwable, Unit] =
    allCatch.either {
      Using.resource(new BufferedWriter(new FileWriter(path.toFile))) { writer =>
        writer.write(data)
      }
    }
