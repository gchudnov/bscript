package com.github.gchudnov.bscript.translator.internal.scala2.laws

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{ LineOps, Transform }
import com.github.gchudnov.bscript.translator.laws.Initializer
import com.github.gchudnov.bscript.translator.TranslateException

/**
 * Initializes types for Scala
 */
final class ScalaInitializer(typeNames: TypeNames, meta: Meta) extends Initializer:

  private val voidTypeName: String = typeNames.voidType
  private val boolTypeName: String = typeNames.boolType
  private val i32TypeName: String  = typeNames.i32Type
  private val i64TypeName: String  = typeNames.i64Type
  private val f32TypeName: String  = typeNames.f32Type
  private val f64TypeName: String  = typeNames.f64Type
  private val decTypeName: String  = typeNames.decType
  private val strTypeName: String  = typeNames.strType

  def init(toType: Type): Either[Throwable, Seq[String]] = toType match
    case ss: SStruct =>
      initStruct(ss)
    case cs: VectorType =>
      initCollection(cs)
    case bs: SBuiltInType =>
      initBuiltInType(bs)
    case other => Left(new TranslateException(s"Cannot initialize Type '${other}'"))

  private def initBuiltInType(bs: SBuiltInType): Either[Throwable, Seq[String]] = bs.name match
    case `voidTypeName` => Right(Seq("()"))
    case `boolTypeName` => Right(Seq("false"))
    case `i32TypeName`  => Right(Seq("0"))
    case `i64TypeName`  => Right(Seq("0L"))
    case `f32TypeName`  => Right(Seq("0.0f"))
    case `f64TypeName`  => Right(Seq("0.0"))
    case `decTypeName`  => Right(Seq("BigDecimal.valueOf(0)"))
    case `strTypeName`  => Right(Seq("\"\""))
    case other          => Left(new TranslateException(s"Cannot initialize BuiltInType '${other}'"))

  private def initCollection(cs: VectorType): Either[Throwable, Seq[String]] =
    Right(Seq("List.empty"))

  private def initStruct(ss: SStruct): Either[Throwable, Seq[String]] =
    for
      fields <- Transform
                  .sequence(meta.symbolsFor(ss).toList.map { case (s) =>
                    s match
                      case x: SVar =>
                        meta
                          .typeFor(x)
                          .left
                          .map(ex => new TranslateException(s"Cannot initialize struct field", ex))
                          .flatMap(t => init(t).map(ls => LineOps.joinCR(" = ", Seq(x.name), LineOps.tabTail(1, ls))))
                      case other =>
                        Left(new TranslateException(s"Cannot initialize struct field that is not a type '${other}'"))
                  })
      lines = LineOps.wrap(s"${ss.name}(", ")", LineOps.wrapEmpty(LineOps.tabLines(1, LineOps.joinVAll(",", fields))))
    yield lines
