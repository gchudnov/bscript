package com.github.gchudnov.bscript.translator.laws

/**
 * Init values for Scala
 * {{{
 *   e.g.
 *   Int we init with 0
 *   Long we init with 0L
 *   ...
 * }}}
 */
trait TypeInit:
  def voidType: String
  def boolType: String
  def i32Type: String
  def i64Type: String
  def f32Type: String
  def f64Type: String
  def decType: String
  def strType: String
  def dateType: String
  def datetimeType: String
