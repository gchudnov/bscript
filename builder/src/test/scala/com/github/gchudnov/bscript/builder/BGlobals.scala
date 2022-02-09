package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.CompiledExpr
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.ShowOps.split

import java.time.{ LocalDate, OffsetDateTime, ZoneId }
import scala.util.control.Exception.allCatch

object BGlobals:

  val typeNames: TypeNames = new TypeNames:
    override def autoType: String     = "auto"
    override def nothingType: String  = "nothing"
    override def voidType: String     = "void"
    override def boolType: String     = "boolean"
    override def i32Type: String      = "int"
    override def i64Type: String      = "long"
    override def f32Type: String      = "float"
    override def f64Type: String      = "double"
    override def decType: String      = "decimal"
    override def strType: String      = "string"
    override def dateType: String     = "date"
    override def datetimeType: String = "datetime"

  def make(): (Meta, SBlock) =
    val g = SBlock("#global")

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

    (meta, g)

  private val utcZone: ZoneId = ZoneId.of("Z")

  private val unitDays: String    = "days"
  private val unitHours: String   = "hours"
  private val unitMinutes: String = "minutes"
  private val unitSeconds: String = "seconds"

  def prelude: Block = Block(
    MethodDecl(
      TypeRef(typeNames.voidType),
      "printf",
      List(
        ArgDecl(TypeRef(typeNames.strType), "format"),
        ArgDecl(TypeRef(typeNames.autoType), "value")
      ),
      Block.empty,
      Seq(ComAnn("prints the formatted string to StdOut"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.i32Type),
      "strLen",
      List(
        ArgDecl(TypeRef(typeNames.strType), "s")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("returns the length of the provided string"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "offsetDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("offsets the provided date-time"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "setDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.i32Type), "offset"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("sets data and time to the specified value"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.i32Type),
      "fieldOfDateTime",
      List(
        ArgDecl(TypeRef(typeNames.datetimeType), "value"),
        ArgDecl(TypeRef(typeNames.strType), "unit")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.i32Type))
      ),
      Seq(ComAnn("return the specified part of date-time as an integer value"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.boolType),
      "isDefined",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.boolType))
      ),
      Seq(ComAnn("returns true of the provided variable is defined, otherwise false"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("x"))),
      "coalesce",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "x"),
        ArgDecl(TypeRef(typeNames.autoType), "y")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("x"))))
      ),
      Seq(ComAnn("returns the first non-null value out of two values that were provided"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.dateType),
      "today",
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.dateType))
      ),
      Seq(ComAnn("returns today as date"), StdAnn())
    ),
    MethodDecl(
      TypeRef(typeNames.datetimeType),
      "now",
      List.empty[ArgDecl],
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = TypeRef(typeNames.datetimeType))
      ),
      Seq(ComAnn("returns current date and time as date-time"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "round",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("rounds the provided value with the given precision"), StdAnn())
    ),
    MethodDecl(
      DeclType(Var(SymbolRef("value"))),
      "truncate",
      List(
        ArgDecl(TypeRef(typeNames.autoType), "value"), // f32, f64, dec
        ArgDecl(TypeRef(typeNames.i32Type), "precision")
      ),
      Block(
        CompiledExpr(callback = CompiledExpr.idCallback, retType = DeclType(Var(SymbolRef("value"))))
      ),
      Seq(ComAnn("truncates the provided value with the given precision"), StdAnn())
    )
  )
