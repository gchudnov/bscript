package com.github.gchudnov.bscript.lang.types

/**
 * Type Names.
 *
 * Used to allow to specify different type names for a type. E.g. integer data type might be i32 or int.
 */
trait TypeNames:
  def autoType: String
  def nothingType: String
  def voidType: String
  def boolType: String
  def i32Type: String
  def i64Type: String
  def f32Type: String
  def f64Type: String
  def decType: String // decimal
  def strType: String
  def dateType: String
  def datetimeType: String

object TypeNames:
  def listAll(tn: TypeNames): List[String] = List(
    tn.autoType,
    tn.nothingType,
    tn.voidType,
    tn.boolType,
    tn.i32Type,
    tn.i64Type,
    tn.f32Type,
    tn.f64Type,
    tn.decType,
    tn.strType,
    tn.dateType,
    tn.datetimeType
  )
