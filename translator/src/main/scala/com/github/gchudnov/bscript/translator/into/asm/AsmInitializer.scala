package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{LineOps, Transform}
import com.github.gchudnov.bscript.translator.TranslateException
import com.github.gchudnov.bscript.translator.laws.{Initializer, TypeInit}

import scala.collection.immutable.Seq

/**
 * Initializes types for C
 */
final class AsmInitializer(typeNames: TypeNames, typeInit: TypeInit, typeNA: TypeInit, meta: Meta) extends Initializer:

  private val voidTypeName: String = typeNames.voidType
  private val boolTypeName: String = typeNames.boolType
  private val i32TypeName: String  = typeNames.i32Type
  private val i64TypeName: String  = typeNames.i64Type
  private val f32TypeName: String  = typeNames.f32Type
  private val f64TypeName: String  = typeNames.f64Type
  private val decTypeName: String  = typeNames.decType
  private val strTypeName: String  = typeNames.strType
  private val dateTypeName: String  = typeNames.dateType
  private val datetimeTypeName: String  = typeNames.datetimeType

  override def init(toType: Type): Either[Throwable, Seq[String]] = toType match
    case ss: SStruct =>
      initStruct(ss)
    case cs: VectorType =>
      initCollection(cs)
    case bs: SBuiltInType =>
      initBuiltInType(bs)
    case other => Left(new TranslateException(s"Cannot initialize Type '${other}'"))

  override def na(forType: Type): Either[Throwable, String] = forType match
    case bs: SBuiltInType =>
      naBuiltInType(bs)
    case other => Left(new TranslateException(s"Cannot set to NA the Type '${other}'"))

  private def initBuiltInType(bs: SBuiltInType): Either[Throwable, Seq[String]] = bs.name match
    case `voidTypeName` => Right(Seq(typeInit.voidType))
    case `boolTypeName` => Right(Seq(typeInit.boolType))
    case `i32TypeName`  => Right(Seq(typeInit.i32Type))
    case `i64TypeName`  => Right(Seq(typeInit.i64Type))
    case `f32TypeName`  => Right(Seq(typeInit.f32Type))
    case `f64TypeName`  => Right(Seq(typeInit.f64Type))
    case `decTypeName`  => Right(Seq(typeInit.decType))
    case `strTypeName`  => Right(Seq(typeInit.strType))
    case other          => Left(new TranslateException(s"Cannot initialize BuiltInType '${other}'"))

  private def naBuiltInType(bs: SBuiltInType): Either[Throwable, String] = bs.name match
    case `i32TypeName`  => Right(typeNA.i32Type)
    case `i64TypeName`  => Right(typeNA.i64Type)
    case `f32TypeName`  => Right(typeNA.f32Type)
    case `f64TypeName`  => Right(typeNA.f64Type)
    case `decTypeName`  => Right(typeNA.decType)
    case `strTypeName`  => Right(typeNA.strType)
    case `dateTypeName`  => Right(typeNA.dateType)
    case `datetimeTypeName`  => Right(typeNA.datetimeType)
    case other          => Left(new TranslateException(s"Cannot reset the BuiltInType '${other}'"))

  private def initCollection(cs: VectorType): Either[Throwable, Seq[String]] =
    Right(Seq("[]"))

  private def initStruct(ss: SStruct): Either[Throwable, Seq[String]] =
    for
      fields <- Transform
                  .sequence(meta.symbolsFor(ss).map { case (s) =>
                    s match
                      case x: SVar =>
                        meta
                          .typeFor(x)
                          .left
                          .map(ex => new TranslateException(s"Cannot initialize struct field", ex))
                          .flatMap(t => init(t).map(ls => LineOps.joinCR(": ", Seq(x.name), LineOps.tabTail(1, ls))))
                      case other =>
                        Left(new TranslateException(s"Cannot initialize struct field that is not a type '${other}'"))
                  })
      lines = LineOps.wrap(s"{", "}", LineOps.wrapEmpty(LineOps.tabLines(1, LineOps.joinVAll(",", fields))))
    yield lines
