package com.github.gchudnov.bscript.b1.cli.display

import com.github.gchudnov.swearwolf.term.*
import com.github.gchudnov.swearwolf.util.*
import com.github.gchudnov.swearwolf.util.TextStyle.*
import com.github.gchudnov.bscript.interpreter.memory.Cell

object CellDisplay:
  import Cell.*

  private val header = "RETURN VALUE"

  def print(cell: Cell): Unit =
    val writer = Writer.syncId(Term.syncId())

    val headerText = Display.padRight(header, Display.lineWidth)
    writer.putLn(headerText, Foreground(Color.Black) | Background(Color.White))

    val cellText = cell.show
    writer.putLn(cellText, Foreground(Color.DodgerBlue))

    writer.putLn("")
