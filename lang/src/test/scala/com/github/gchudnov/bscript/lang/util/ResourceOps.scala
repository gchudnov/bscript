package com.github.gchudnov.bscript.lang.util

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

  def resourceSave(resourcePath: String, dest: Path): Either[Throwable, Long] =
    allCatch.either {
      val classLoader: ClassLoader = getClass.getClassLoader
      Using.resources(classLoader.getResourceAsStream(resourcePath), new FileOutputStream(dest.toFile)) { (inStream, outStream) =>
        inStream.transferTo(outStream)
      }
    }.left.map(t => new RuntimeException(s"Cannot save the resource '${resourcePath}' to '${dest}'", t))
