package com.github.gchudnov.bscript.rewriter.internal

final case class FilterState()

object FilterState:
  def make(): FilterState = 
    FilterState()