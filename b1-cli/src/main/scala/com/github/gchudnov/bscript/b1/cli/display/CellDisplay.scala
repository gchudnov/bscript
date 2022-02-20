package com.github.gchudnov.bscript.b1.cli.display

import com.diogonunes.jcolor.Ansi.colorize
import com.diogonunes.jcolor.Attribute.*
import com.github.gchudnov.bscript.interpreter.memory.Cell

object CellDisplay:
  import Cell.*

  private val header = "RETURN VALUE"

  def print(cell: Cell): Unit =
    Console.out.println(colorize(Display.padRight(header, Display.lineWidth), BRIGHT_BLACK_TEXT(), BRIGHT_WHITE_BACK()))
    Console.out.println(colorize(cell.show, BRIGHT_BLUE_TEXT()))
    Console.out.println()
