package com.github.gchudnov.bscript.interpreter.laws

import com.github.gchudnov.bscript.interpreter.laws.Initializer
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.util.Transform

final class IBasicInitializer(types: Types, meta: Meta) extends Initializer:

  private val voidTypeName: String = types.voidType.name
  private val boolTypeName: String = types.boolType.name
  private val i32TypeName: String  = types.i32Type.name
  private val i64TypeName: String  = types.i64Type.name
  private val f32TypeName: String  = types.f32Type.name
  private val f64TypeName: String  = types.f64Type.name
  private val decTypeName: String  = types.decType.name
  private val strTypeName: String  = types.strType.name

  override def init(toType: Type): Either[MemoryException, Cell] = toType match
    case ss: SStruct =>
      initStruct(ss)
    case cs: VectorType =>
      initCollection(cs)
    case bs: SBuiltInType =>
      initBuiltInType(bs)
    case other => Left(new MemoryException(s"Cannot initialize Type '${other}'"))

  private def initBuiltInType(bs: SBuiltInType): Either[MemoryException, Cell] = bs.name match
    case `voidTypeName` => Right(VoidCell)
    case `boolTypeName` => Right(BoolCell(false))
    case `i32TypeName`  => Right(IntCell(0))
    case `i64TypeName`  => Right(LongCell(0L))
    case `f32TypeName`  => Right(FloatCell(0.0f))
    case `f64TypeName`  => Right(DoubleCell(0.0))
    case `decTypeName`  => Right(DecimalCell(BigDecimal.valueOf(0)))
    case `strTypeName`  => Right(StrCell(""))
    case other          => Left(new MemoryException(s"Cannot initialize BuiltInType '${other}'"))

  private def initCollection(cs: VectorType): Either[MemoryException, Cell] =
    Right(VecCell(List.empty[Cell]))

  private def initStruct(ss: SStruct): Either[MemoryException, Cell] =
    Transform
      .sequence(meta.symbolsFor(ss).toList.map { case (s) =>
        s match
          case x: SVar =>
            meta
              .typeFor(x)
              .left
              .map(ex => new MemoryException(s"Cannot initialize struct field '${ss.name}.${x.name}'", ex))
              .flatMap(t => init(t).map(c => (x.name, c)))
          case other =>
            Left(new MemoryException(s"Cannot initialize struct field '${ss.name}.${s.name}' that is not SVar: '${other}'"))
      })
      .map(xs => StructCell(xs.toMap))
