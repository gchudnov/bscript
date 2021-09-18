package com.github.gchudnov.bscript.lang.util

trait Show[A]:
  def show(a: A): String

object Show:
  implicit class ShowOps[A](a: A)(implicit ev: Show[A]):
    def show(): String = ev.show(a)
