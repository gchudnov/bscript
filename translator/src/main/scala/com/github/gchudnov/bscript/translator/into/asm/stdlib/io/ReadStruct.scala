package com.github.gchudnov.bscript.translator.into.asm.stdlib.io

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState, AsmTypeNA}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split

import scala.collection.immutable.Seq

/**
 * Reads the provided structure from a string
 * @param struct structure declaration with resolved types
 * @param typeNames type names
 */
final class ReadStruct(struct: StructDecl, typeNames: TypeNames) {

  private val updateFnName: String =
    s"update_${struct.name}"

  private val readFnName: String =
    s"read_${struct.name}"

  // field name -> field type
  private val fields: Seq[(String, String)] =
    struct.fields.map(fd => (fd.name, fd.fType.name))

  // type -> NA
  private val na: Map[String, String] =
    Map(
      typeNames.i32Type -> AsmTypeNA.i32Type,
      typeNames.i64Type -> AsmTypeNA.i64Type,
      typeNames.f32Type -> AsmTypeNA.f32Type,
      typeNames.f64Type -> AsmTypeNA.f64Type,
      typeNames.decType -> AsmTypeNA.decType,
      typeNames.strType -> AsmTypeNA.strType,
      typeNames.dateType -> AsmTypeNA.dateType,
      typeNames.datetimeType -> AsmTypeNA.datetimeType,
      typeNames.boolType -> AsmTypeNA.boolType
    )

  // type -> parser
  private val parsers: Map[String, String] =
    Map(
      typeNames.i32Type -> "I32.parseInt(value)",
      typeNames.i64Type -> "I64.parseInt(value)",
      typeNames.f32Type -> "F32.parseFloat(value)",
      typeNames.f64Type -> "F64.parseFloat(value)",
      typeNames.decType -> "F64.parseFloat(value)",
      typeNames.strType -> "value",
      typeNames.dateType -> "Date.parse(value)",
      typeNames.datetimeType -> "Date.parse(value)",
      typeNames.boolType -> "value == \"true\""
    )

  private val conds: Expr =
    fields
      .foldLeft(None: Option[If]){case (acc, (fName, fType)) => {
        val if1 = If(Equal(StrVal("key"), StrVal(fName)), Block(Assign(Access(Var(SymbolRef("d")), Var(SymbolRef(fName))), CompiledExpr({
          case s: AsmState =>
            Right(s.copy(lines = Seq(parsers(fType))))
          case other =>
            Left(new AsmException(s"Unexpected state passed to ${updateFnName}: ${other}"))
        }, TypeRef(fType)))))
        acc match {
          case Some(if0) =>
            Some(if0.copy(else1 = Some(if1)))
          case None =>
            Some(if1)
        }
    }}.getOrElse(Block.empty)

  def updateDecl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.voidType),
      updateFnName,
      List(
        ArgDecl(TypeRef(struct.name), "d"),
        ArgDecl(TypeRef(typeNames.strType), "key"),
        ArgDecl(TypeRef(typeNames.strType), "value")
      ),
      Block(
        conds
      ),
      Seq(ComAnn(s"Update the key in ${struct.name} data structure with the provided value"), StdAnn())
    )

  def readDecl: MethodDecl =
    MethodDecl(
      TypeRef(struct.name),
      readFnName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "input"),
      ),
      Block(
        VarDecl(
          TypeRef(struct.name),
          "d",
          StructVal(
            TypeRef(struct.name),
            fields.map((fName, _) => fName -> NothingVal()).toMap
          )
        ),
        CompiledExpr(callback = this.read, retType = TypeRef(struct.name))
      ),
      Seq(ComAnn(s"Read the ${struct.name} data structure from a string"), StdAnn())
    )

  private def read(s: Any): Either[Throwable, Any] =
    s match
      case s: AsmState =>
        for lines <- Right(
          split(
            s"""const lines = input.split("\\n");
               |for (let i = 0; i < lines.length; i++) {
               |  const line = lines[i];
               |  if (line.length == 0) {
               |    // empty line
               |    continue
               |  }
               |
               |  const kv = line.split("=");
               |  if (kv.length != 2) {
               |    // invalid line
               |    continue;
               |  }
               |
               |  const key = kv[0].trim();
               |  const value = kv[1].trim();
               |  ${updateFnName}(d, key, value);
               |}
               |
               |return d;
               |""".stripMargin
          )
        )
        yield s.copy(
          lines = lines,
          imports = s.imports,
          inits = s.inits
        )

      case other =>
        Left(new AsmException(s"Unexpected state passed to ${updateFnName}: ${other}"))

}
