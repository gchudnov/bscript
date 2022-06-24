package com.github.gchudnov.bscript.b1.cli.display

import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatchDiff
import com.github.gchudnov.bscript.interpreter.memory.Diff
import com.github.gchudnov.bscript.interpreter.memory.Cell

import com.github.gchudnov.swearwolf.rich.IdRichText.*
import com.github.gchudnov.swearwolf.rich.RichText
import com.github.gchudnov.swearwolf.term.*
import com.github.gchudnov.swearwolf.util.*
import com.github.gchudnov.swearwolf.util.TextStyle.*

object MemWatchDisplay:
  import Cell.*

  private val header = "WATCH LOG"

  def print(log: Seq[MemWatchDiff]): Unit =
    val writer = Writer.syncId(Term.syncId())

    val headerText = Display.padRight(header, Display.lineWidth)
    writer.putLn(headerText, Foreground(Color.Black) | Background(Color.White))

    log.foreach(it =>
      val methodName = it.name
      val path       = it.path.value
      val diffs      = it.diffs

      // method
      val methodFgColor = if (diffs.isEmpty) then Foreground(Color.White) else Foreground(Color.Yellow)
      val methodLine    = s"method: $methodName() | watch: '$path'"
      val methodText    = Display.padRight(methodLine, Display.lineWidth)
      writer.putLn(methodText, methodFgColor)

      // diffs
      val maxKeyWidth = if diffs.nonEmpty then diffs.map(_.key.length).max else 0
      diffs.foreach(diff =>
        val (line, fgColor) = diff match
          case Diff.Removed(key, value) =>
            val line  = s"  - ${Display.padRight(singleQuote(key), maxKeyWidth + 2)} | ${value.asInstanceOf[Cell].show}"
            val color = Foreground(Color.OrangeRed)
            (line, color)
          case Diff.Added(key, value) =>
            val line  = s"  + ${Display.padRight(singleQuote(key), maxKeyWidth + 2)} | ${value.asInstanceOf[Cell].show}"
            val color = Foreground(Color.GreenYellow)
            (line, color)
          case Diff.Updated(key, before, after) =>
            val line  = s"  * ${Display.padRight(singleQuote(key), maxKeyWidth + 2)} | ${before.asInstanceOf[Cell].show} -> ${after.asInstanceOf[Cell].show}"
            val color = Foreground(Color.Gold)
            (line, color)

        writer.putLn(line, fgColor)
      )

      writer.putLn("")
    )

  private def singleQuote(s: String): String =
    "'" + s + "'"
