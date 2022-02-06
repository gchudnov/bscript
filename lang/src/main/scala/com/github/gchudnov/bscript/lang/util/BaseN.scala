package com.github.gchudnov.bscript.lang.util

import scala.annotation.tailrec

trait BaseN:
  def base: Long

  def lookup(x: Long): Char
  def reverseLookup(c: Char): Long

  def encode(x: Long): String =
    @tailrec
    def iterate(n: Long, ds: List[Char]): List[Char] =
      assert(n >= 0, s"'n' cannot be negative, got ${n}")
      if n == 0 then ds
      else
        // n > 0
        val d = lookup(n % base)
        iterate(n / base, d :: ds)

    val cs =
      if x == 0 then List(lookup(x))
      else iterate(x, List.empty[Char])

    cs.mkString

  def decode(s: String): Long =

    @tailrec
    def iterate(acc: Long, ds: List[Char]): Long =
      if ds.isEmpty then acc
      else
        val d :: tail = ds
        val n         = acc * base + reverseLookup(d)
        iterate(n, tail)

    iterate(0L, s.toList)
