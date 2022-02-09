package com.github.gchudnov.bscript.builder.util

object Base26 extends BaseN:
  override def base: Long = 26

  override def lookup(x: Long): Char =
    ('a'.toLong + x).toChar

  override def reverseLookup(c: Char): Long =
    c.toLong - 'a'.toLong
