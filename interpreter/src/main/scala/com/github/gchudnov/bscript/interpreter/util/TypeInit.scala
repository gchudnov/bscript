package com.github.gchudnov.bscript.interpreter.util

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.interpreter.memory.Cell
import com.github.gchudnov.bscript.interpreter.InterpreterException
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.types.TypeName
import java.time.OffsetDateTime
import java.time.LocalDate

/**
 * Type Initialization
 *
 * Init() -> Cell(?)
 */
private[interpreter] object TypeInit:

  private val builtInCellMap: Map[String, Cell] = Map(
    TypeName.nothing  -> Cell.nothing,
    TypeName.void     -> Cell.void,
    TypeName.bool     -> Cell.bool(false),
    TypeName.u8       -> Cell.u8(0),
    TypeName.i16      -> Cell.i16(0),
    TypeName.i32      -> Cell.i32(0),
    TypeName.i64      -> Cell.i64(0L),
    TypeName.f32      -> Cell.f32(0.0f),
    TypeName.f64      -> Cell.f64(0.0),
    TypeName.dec      -> Cell.decimal(BigDecimal.valueOf(0)),
    TypeName.chr      -> Cell.chr(0),
    TypeName.str      -> Cell.str(""),
    TypeName.date     -> Cell.date(LocalDate.parse("1970-01-01")),
    TypeName.datetime -> Cell.datetime(OffsetDateTime.parse("1970-01-01T00:00:00+00:00")),
  )

  def init(t: TypeAST): Cell =
    println(t)
    t match
      case _: Auto =>
        throw new InterpreterException("Auto type is not supported for initialization.")
      case _: TypeId =>
        throw new InterpreterException("TypeId type is not supported for initialization.")
      case _: ByName =>
        throw new InterpreterException("ByName type is not supported for initialization.")
      case x: RealType =>
        initRealType(x)

  /**
   * Initialize the real type.
   */
  private def initRealType(realType: RealType): Cell =
    realType match
      case x: BuiltInType =>
        initBuiltInType(x)
      case _: GenericType =>
        throw new InterpreterException("GenericType type is not supported for initialization.")
      case _: MethodType =>
        throw new InterpreterException("MethodType type is not supported for initialization.")
      case s: StructType =>
        initStructType(s)
      case m: MapType =>
        initMapType(m)
      case s: SetType =>
        initSetType(s)
      case v: VecType =>
        initVecType(v)

  /**
   * Initialize the built-in type.
   */
  private def initBuiltInType(x: BuiltInType): Cell =
    builtInCellMap.getOrElse(x.name, throw new InterpreterException(s"Cannot initialize BuiltInType '${x.name}', type is not supported"))

  /**
   * Initialize the struct type.
   *
   * TODO: consider generics
   */
  private def initStructType(s: StructType): Cell =
    val m = s.fields.foldLeft(Map.empty[String, Cell]) { case (acc, f) =>
      val name = f.name
      val t    = f.aType // TODO: it must be eval-type
      val c    = init(t)
      acc + (name -> c)
    }
    Cell.struct(m)

  /**
   * Initialize the map type.
   */
  private def initMapType(m: MapType): Cell =
    Cell.map(List.empty[(Cell, Cell)])

  /**
   * Initialize the set type.
   */
  private def initSetType(s: SetType): Cell =
    Cell.set(List.empty[Cell])

  /**
   * Initialize the vector type.
   */
  private def initVecType(v: VecType): Cell =
    Cell.vec(List.empty[Cell])
