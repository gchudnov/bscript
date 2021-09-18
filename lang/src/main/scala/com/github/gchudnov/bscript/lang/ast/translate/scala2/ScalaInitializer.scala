package com.github.gchudnov.bscript.lang.ast.translate.scala2

import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{ ShowOps, Transform }

/**
 * Initializes types for Scala
 */
final class ScalaInitializer(typeNames: TypeNames, meta: Meta):

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
    case other => Left(new Scala2Exception(s"Cannot initialize Type '${other}'"))

  private def initBuiltInType(bs: SBuiltInType): Either[Throwable, Seq[String]] = bs.name match
    case `voidTypeName` => Right(Seq("()"))
    case `boolTypeName` => Right(Seq("false"))
    case `i32TypeName`  => Right(Seq("0"))
    case `i64TypeName`  => Right(Seq("0L"))
    case `f32TypeName`  => Right(Seq("0.0f"))
    case `f64TypeName`  => Right(Seq("0.0"))
    case `decTypeName`  => Right(Seq("BigDecimal.valueOf(0)"))
    case `strTypeName`  => Right(Seq("\"\""))
    case other          => Left(new Scala2Exception(s"Cannot initialize BuiltInType '${other}'"))

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
                          .map(ex => new Scala2Exception(s"Cannot initialize struct field", ex))
                          .flatMap(t => init(t).map(ls => ShowOps.joinCR(" = ", Seq(x.name), ShowOps.tabTail(1, ls))))
                      case other =>
                        Left(new Scala2Exception(s"Cannot initialize struct field that is not a type '${other}'"))
                  })
      lines = ShowOps.wrap(s"${ss.name}(", ")", ShowOps.wrapEmpty(ShowOps.tabLines(1, ShowOps.joinVAll(",", fields))))
    yield lines
