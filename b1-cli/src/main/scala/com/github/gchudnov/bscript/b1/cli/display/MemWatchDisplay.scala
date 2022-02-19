package com.github.gchudnov.bscript.b1.cli.display

import com.diogonunes.jcolor.Ansi.colorize
import com.diogonunes.jcolor.Attribute.*
import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatchDiff
import com.github.gchudnov.bscript.interpreter.memory.Diff
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.lang.util.Show.*

object MemWatchDisplay:
  import Cell.*

  private val header = "WATCH LOG"

  Console.out.println(colorize(Display.padRight(header, Display.lineWidth), BRIGHT_BLACK_TEXT(), BRIGHT_WHITE_BACK()))

  def print(log: Seq[MemWatchDiff]): Unit =
    log.foreach(it => {
      val methodName = it.name
      val path = it.path.value
      val diffs = it.diffs

      // method
      val methodFgColor = if (diffs.isEmpty) then WHITE_TEXT() else YELLOW_TEXT()
      val methodLine = s"method: '$methodName' | watch: '$path'"
      Console.out.println(colorize(Display.padRight(methodLine, Display.lineWidth), methodFgColor))

      // diffs
      diffs.foreach(diff => {
        val (line, fgColor) = diff match {
          case Diff.Removed(key, value) =>
            val line = s"  - '${key}' | ${value.asInstanceOf[Cell].show()}"
            val color = BRIGHT_RED_TEXT()
            (line, color)
          case Diff.Added(key, value) =>
            val line = s"  + '${key}' | ${value.asInstanceOf[Cell].show()}"
            val color = BRIGHT_GREEN_TEXT()
            (line, color)            
          case Diff.Updated(key, before, after) =>
            val line = s"  * '${key}' | ${before.asInstanceOf[Cell].show()} -> ${after.asInstanceOf[Cell].show()}"
            val color = BRIGHT_YELLOW_TEXT()
            (line, color)            
        }

        Console.out.println(colorize(line, fgColor))
      })

      Console.out.println()
    })
