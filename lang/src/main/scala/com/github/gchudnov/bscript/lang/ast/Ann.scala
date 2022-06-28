package com.github.gchudnov.bscript.lang.ast

/**
 * Annotation to apply to an AST element
 */
trait Ann:
  def value: String

/**
 * Comment applied to AST
 */
final case class ComAnn(value: String) extends Ann

/**
 * Marks code, related to standard-library.
 *
 * Used, for example, to prevent serialization of [std] functions.
 */
final case class StdAnn() extends Ann:
  import StdAnn.*
  override val value: String = marker

object StdAnn:
  val marker: String = "[std]"
