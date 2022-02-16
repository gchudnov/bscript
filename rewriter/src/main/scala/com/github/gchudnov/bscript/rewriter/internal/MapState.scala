package com.github.gchudnov.bscript.rewriter.internal

final case class MapState()

private[rewriter] object MapState:
  def make(): MapState =
    MapState()
