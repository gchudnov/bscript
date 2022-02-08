package com.github.gchudnov.bscript.lang.types

import com.github.gchudnov.bscript.lang.symbols.SBuiltInType

sealed trait Types:
  def autoType: SBuiltInType
  def nothingType: SBuiltInType
  def voidType: SBuiltInType
  def boolType: SBuiltInType
  def i32Type: SBuiltInType
  def i64Type: SBuiltInType
  def f32Type: SBuiltInType
  def f64Type: SBuiltInType
  def decType: SBuiltInType
  def strType: SBuiltInType
  def dateType: SBuiltInType
  def datetimeType: SBuiltInType

object Types:
  import VisitorOps.*

  def make(typeNames: TypeNames): Types =
    val sAutoType     = SBuiltInType(typeNames.autoType)
    val sNothingType  = SBuiltInType(typeNames.nothingType)
    val sVoidType     = SBuiltInType(typeNames.voidType)
    val sBoolType     = SBuiltInType(typeNames.boolType)
    val sI32Type      = SBuiltInType(typeNames.i32Type)
    val sI64Type      = SBuiltInType(typeNames.i64Type)
    val sF32Type      = SBuiltInType(typeNames.f32Type)
    val sF64Type      = SBuiltInType(typeNames.f64Type)
    val sDecType      = SBuiltInType(typeNames.decType)
    val sStrType      = SBuiltInType(typeNames.strType)
    val sDateType     = SBuiltInType(typeNames.dateType)
    val sDatetimeType = SBuiltInType(typeNames.datetimeType)

    new Types:
      override def autoType: SBuiltInType     = sAutoType
      override def nothingType: SBuiltInType  = sNothingType
      override def voidType: SBuiltInType     = sVoidType
      override def boolType: SBuiltInType     = sBoolType
      override def i32Type: SBuiltInType      = sI32Type
      override def i64Type: SBuiltInType      = sI64Type
      override def f32Type: SBuiltInType      = sF32Type
      override def f64Type: SBuiltInType      = sF64Type
      override def decType: SBuiltInType      = sDecType
      override def strType: SBuiltInType      = sStrType
      override def dateType: SBuiltInType     = sDateType
      override def datetimeType: SBuiltInType = sDatetimeType
