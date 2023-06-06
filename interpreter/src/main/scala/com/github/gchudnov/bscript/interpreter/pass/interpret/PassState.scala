package com.github.gchudnov.bscript.interpreter.pass.interpret

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.builder.util.Base26

/**
 * Pass State to use in the interpreter.
 *
 * @param area
 *   current memory area
 * @param retValue
 *   return value
 */
private[interpret] final case class PassState(
  area: Area,
  retValue: Cell,
):
  /**
   * Pushes a new area.
   *
   * @return
   *   a new state with a new area
   */
  def pushArea(): PassState =
    val id = Base26.inc(area.name)
    this.copy(area = area.push(id))

  /**
   * Pops the current area.
   *
   * @return
   *   a new state with the current area removed
   */
  def popArea(): PassState =
    this.copy(area = area.pop().get)

  /**
   * Sets the return value.
   *
   * @param value
   *   return value
   * @return
   *   a new state with the return value
   */
  def withRetValue(value: Cell): PassState =
    this.copy(retValue = value)

object PassState:

  def from(in: InState): PassState =
    PassState(
      area = Area("a"),
      retValue = Cell.Void,
    )

  def into(state: PassState): OutState =
    OutState(
      retValue = state.retValue,
    )
