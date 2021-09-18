package com.github.gchudnov.bscript.lang.util

trait Empty[A]:
  def empty: A

object Empty:
  implicit class EmptyOps[A](a: A)(implicit ev: Empty[A]):
    def empty: A = ev.empty
