package com.github.gchudnov.bscript.rewriter.internal

final case class FilterState()

private[rewriter] object FilterState:
  def make(): FilterState =
    FilterState()
