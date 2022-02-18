package com.github.gchudnov.bscript.inspector.internal.dbglib

import com.github.gchudnov.bscript.interpreter.internal.StashEntry
import com.github.gchudnov.bscript.interpreter.internal.Stash
import com.github.gchudnov.bscript.inspector.InspectorException
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import com.github.gchudnov.bscript.interpreter.memory.Diff

import scala.collection.mutable.Stack

final case class MemWatchDiff(
  name: String,
  path: CellPath,
  diffs: List[Diff.Change[String, Cell]]
)

final case class MemWatchStashEntry(
  calls: Map[String, Stack[Option[Cell]]],
  log: Vector[MemWatchDiff]
) extends StashEntry

object MemWatchStashEntry:
  val name: String = "memWatch"

  val empty: MemWatchStashEntry =
    new MemWatchStashEntry(Map.empty[String, Stack[Option[Cell]]], Vector.empty[MemWatchDiff])

  def get(stash: Stash): MemWatchStashEntry =
    stash.value
      .getOrElse(MemWatchStashEntry.name, MemWatchStashEntry.empty)
      .asInstanceOf[MemWatchStashEntry] // NOTE: we do unsafe cast here considering that we know the type
