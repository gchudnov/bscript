package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.types.TypeName

import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import scala.util.control.Exception.allCatch
import com.github.gchudnov.bscript.builder.visitors.ScopeBuilder

/**
  * Globals for building
  */
object BGlobals:

  def make(sb: ScopeBuilder): ScopeBuilder =

    // NOTE: we're not defining any new scopes here, thus the scope should be defined before calling `.make()`
    sb
      .define(SBuiltIn.auto)
      .define(SBuiltIn.nothing)
      .define(SBuiltIn.void)
      .define(SBuiltIn.bool)
      .define(SBuiltIn.i32)
      .define(SBuiltIn.i64)
      .define(SBuiltIn.f32)
      .define(SBuiltIn.f64)
      .define(SBuiltIn.dec)
      .define(SBuiltIn.str)
      .define(SBuiltIn.date)
      .define(SBuiltIn.datetime)

//   def prelude: Block = Block(
//     MethodDecl(
//       TypeRef(typeNames.voidType),
//       "printf",
//       List(
//         ArgDecl(TypeRef(typeNames.strType), "format"),
//         ArgDecl(TypeRef(typeNames.autoType), "value")
//       ),
//       Block.empty,
//       Seq(ComAnn("prints the formatted string to StdOut"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.i32Type),
//       "strLen",
//       List(
//         ArgDecl(TypeRef(typeNames.strType), "s")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
//       ),
//       Seq(ComAnn("returns the length of the provided string"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.datetimeType),
//       "offsetDateTime",
//       List(
//         ArgDecl(TypeRef(typeNames.datetimeType), "value"),
//         ArgDecl(TypeRef(typeNames.i32Type), "offset"),
//         ArgDecl(TypeRef(typeNames.strType), "unit")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
//       ),
//       Seq(ComAnn("offsets the provided date-time"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.datetimeType),
//       "setDateTime",
//       List(
//         ArgDecl(TypeRef(typeNames.datetimeType), "value"),
//         ArgDecl(TypeRef(typeNames.i32Type), "offset"),
//         ArgDecl(TypeRef(typeNames.strType), "unit")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
//       ),
//       Seq(ComAnn("sets data and time to the specified value"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.i32Type),
//       "fieldOfDateTime",
//       List(
//         ArgDecl(TypeRef(typeNames.datetimeType), "value"),
//         ArgDecl(TypeRef(typeNames.strType), "unit")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
//       ),
//       Seq(ComAnn("return the specified part of date-time as an integer value"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.boolType),
//       "isDefined",
//       List(
//         ArgDecl(TypeRef(typeNames.autoType), "x")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.boolType))
//       ),
//       Seq(ComAnn("returns true of the provided variable is defined, otherwise false"), StdAnn())
//     ),
//     MethodDecl(
//       DeclType(Var(SymbolRef("x"))),
//       "coalesce",
//       List(
//         ArgDecl(TypeRef(typeNames.autoType), "x"),
//         ArgDecl(TypeRef(typeNames.autoType), "y")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("x"))))
//       ),
//       Seq(ComAnn("returns the first non-null value out of two values that were provided"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.dateType),
//       "today",
//       List.empty[ArgDecl],
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.dateType))
//       ),
//       Seq(ComAnn("returns today as date"), StdAnn())
//     ),
//     MethodDecl(
//       TypeRef(typeNames.datetimeType),
//       "now",
//       List.empty[ArgDecl],
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
//       ),
//       Seq(ComAnn("returns current date and time as date-time"), StdAnn())
//     ),
//     MethodDecl(
//       DeclType(Var(SymbolRef("value"))),
//       "round",
//       List(
//         ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
//         ArgDecl(TypeRef(typeNames.i32Type), "precision")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
//       ),
//       Seq(ComAnn("rounds the provided value with the given precision"), StdAnn())
//     ),
//     MethodDecl(
//       DeclType(Var(SymbolRef("value"))),
//       "truncate",
//       List(
//         ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
//         ArgDecl(TypeRef(typeNames.i32Type), "precision")
//       ),
//       Block(
//         CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
//       ),
//       Seq(ComAnn("truncates the provided value with the given precision"), StdAnn())
//     )
//   )
