package com.github.gchudnov.bscript.builder.util

import scala.annotation.tailrec

sealed trait BaseN:
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
      ds match
        case Nil =>
          acc
        case d :: tail =>
          val n = acc * base + reverseLookup(d)
          iterate(n, tail)

    iterate(0L, s.toList)

/**
 * Base 26
 */
object Base26 extends BaseN:
  override def base: Long = 26

  override def lookup(x: Long): Char =
    ('a'.toLong + x).toChar

  override def reverseLookup(c: Char): Long =
    c.toLong - 'a'.toLong

/**
 * Base 2
 */
object Base2 extends BaseN:
  override def base: Long = 2

  override def lookup(x: Long): Char = x match
    case 0 => 'f';
    case _ => 't'

  override def reverseLookup(c: Char): Long = c match
    case 'f' => 0L;
    case _   => 1L
