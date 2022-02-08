package com.github.gchudnov.bscript.builder.internal

import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{ SBlock, SBuiltInType }

object RootScope:

  def make(name: String, typeNames: TypeNames): Meta =
    val g = SBlock(name)

    val autoType     = SBuiltInType(typeNames.autoType)
    val nothingType  = SBuiltInType(typeNames.nothingType)
    val voidType     = SBuiltInType(typeNames.voidType)
    val boolType     = SBuiltInType(typeNames.boolType)
    val i32Type      = SBuiltInType(typeNames.i32Type)
    val i64Type      = SBuiltInType(typeNames.i64Type)
    val f32Type      = SBuiltInType(typeNames.f32Type)
    val f64Type      = SBuiltInType(typeNames.f64Type)
    val decType      = SBuiltInType(typeNames.decType)
    val strType      = SBuiltInType(typeNames.strType)
    val dateType     = SBuiltInType(typeNames.dateType)
    val datetimeType = SBuiltInType(typeNames.datetimeType)

    val meta =
      Meta.empty
        .defineBlock(g)
        .defineBuiltInType(autoType, g)
        .defineBuiltInType(nothingType, g)
        .defineBuiltInType(voidType, g)
        .defineBuiltInType(boolType, g)
        .defineBuiltInType(i32Type, g)
        .defineBuiltInType(i64Type, g)
        .defineBuiltInType(f32Type, g)
        .defineBuiltInType(f64Type, g)
        .defineBuiltInType(decType, g)
        .defineBuiltInType(strType, g)
        .defineBuiltInType(dateType, g)
        .defineBuiltInType(datetimeType, g)

    meta
