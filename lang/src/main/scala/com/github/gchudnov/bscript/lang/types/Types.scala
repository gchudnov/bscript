package com.github.gchudnov.bscript.lang.types

import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{ ScopeStateException, Symbol, Type }
import com.github.gchudnov.bscript.lang.util.Transform

sealed trait Types:
  def autoType: Symbol with Type
  def nothingType: Symbol with Type
  def voidType: Symbol with Type
  def boolType: Symbol with Type
  def i32Type: Symbol with Type
  def i64Type: Symbol with Type
  def f32Type: Symbol with Type
  def f64Type: Symbol with Type
  def decType: Symbol with Type // decimal
  def strType: Symbol with Type
  def dateType: Symbol with Type
  def datetimeType: Symbol with Type

object Types:
  import VisitorOps.*

  def make(meta: Meta, typeNames: TypeNames): Either[Throwable, Types] =
    for
      types <- Transform.sequence(
                 TypeNames
                   .listAll(typeNames)
                   .map { typeName =>
                     // NOTE: symbolScopes should not contain duplicate names listed in in `typeNames`.
                     meta.symbolScopes
                       .find(_._1.value.name == typeName)
                       .toRight(new ScopeStateException(s"Cannot find Symbol ${typeName} in ScopeState"))
                       .map(_._1.value)
                       .flatMap(_.asType)
                       .map(t => (typeName, t))
                   }
               )
      typeMap               = types.toMap
      resolvedAutoType     <- nameToType(typeMap, typeNames.autoType)
      resolvedNothingType  <- nameToType(typeMap, typeNames.nothingType)
      resolvedVoidType     <- nameToType(typeMap, typeNames.voidType)
      resolvedBoolType     <- nameToType(typeMap, typeNames.boolType)
      resolvedI32Type      <- nameToType(typeMap, typeNames.i32Type)
      resolvedI64Type      <- nameToType(typeMap, typeNames.i64Type)
      resolvedF32Type      <- nameToType(typeMap, typeNames.f32Type)
      resolvedF64Type      <- nameToType(typeMap, typeNames.f64Type)
      resolvedDecType      <- nameToType(typeMap, typeNames.decType)
      resolvedStrType      <- nameToType(typeMap, typeNames.strType)
      resolvedDateType     <- nameToType(typeMap, typeNames.dateType)
      resolvedDatetimeType <- nameToType(typeMap, typeNames.datetimeType)
    yield new Types:
      override def autoType: Symbol with Type     = resolvedAutoType
      override def nothingType: Symbol with Type  = resolvedNothingType
      override def voidType: Symbol with Type     = resolvedVoidType
      override def boolType: Symbol with Type     = resolvedBoolType
      override def i32Type: Symbol with Type      = resolvedI32Type
      override def i64Type: Symbol with Type      = resolvedI64Type
      override def f32Type: Symbol with Type      = resolvedF32Type
      override def f64Type: Symbol with Type      = resolvedF64Type
      override def decType: Symbol with Type      = resolvedDecType
      override def strType: Symbol with Type      = resolvedStrType
      override def dateType: Symbol with Type     = resolvedDateType
      override def datetimeType: Symbol with Type = resolvedDatetimeType

  private def nameToType(typeMap: Map[String, Symbol with Type], name: String): Either[Throwable, Symbol with Type] =
    typeMap.get(name).toRight(new ScopeStateException(s"Type ${name} is not found in TypeMap."))
