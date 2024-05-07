package com.github.gchudnov.bscript.translator.into.asm.stdlib.io

import com.github.gchudnov.bscript.translator.into.asm.{AsmException, AsmState, AsmTypeNA}
import com.github.gchudnov.bscript.translator.into.asm.stdlib.Inits
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.LineOps.split

import scala.collection.immutable.Seq
/**
 * Writes struct to a string
 * @param struct struct declaration
 * @param typeNames type names
 */
final class WriteStruct(struct: StructDecl, typeNames: TypeNames) {

  private val fnName: String =
    s"write_${struct.name}"

  private val fields: Seq[(String, String)] =
    struct.fields.map(fd => (fd.name, fd.fType.name))

  private val asStrings: Map[String, String] =
    Map(
      typeNames.i32Type -> "%s.toString()",
      typeNames.i64Type -> "%s.toString()",
      typeNames.f32Type -> "%s.toString()",
      typeNames.f64Type -> "%s.toString()",
      typeNames.decType -> "%s.toString()",
      typeNames.strType -> "%s",
      typeNames.dateType -> "%s.toISOString()",
      typeNames.datetimeType -> "%s.toISOString()",
      typeNames.boolType -> "%s.toString()"
    )

  private val writes: Seq[Expr] =
    fields.map((fName, fType) => {
      If(Call(SymbolRef("isDefined"), List(Access(Var(SymbolRef("d")), Var(SymbolRef(fName))))), Block(
        Assign(Var(SymbolRef("res")), Add(Var(SymbolRef("res")), CompiledExpr({
          case s: AsmState =>
            Right(s.copy(lines = Seq(asStrings(fType).format(s"d.${fName}"))))
          case other =>
            Left(new AsmException(s"Unexpected state passed to ${fnName}: ${other}"))
        }, TypeRef(typeNames.strType)))),
        Assign(Var(SymbolRef("res")), Add(Var(SymbolRef("res")), StrVal("\\n")))
      ))
    })
//    fields
//      .foldLeft(None: Option[If]){case (acc, (fName, fType)) => {
//        val if1 = If(Equal(StrVal("key"), StrVal(fName)), Block(Assign(Access(Var(SymbolRef("d")), Var(SymbolRef(fName))), CompiledExpr({
//          case s: AsmState =>
//            Right(s.copy(lines = Seq(parsers(fType))))
//          case other =>
//            Left(new AsmException(s"Unexpected state passed to ${updateFnName}: ${other}"))
//        }, TypeRef(fType)))))
//        acc match {
//          case Some(if0) =>
//            Some(if0.copy(else1 = Some(if1)))
//          case None =>
//            Some(if1)
//        }
//      }}.getOrElse(Block.empty)

  def decl: MethodDecl =
    MethodDecl(
      TypeRef(typeNames.strType),
      fnName,
      List(
        ArgDecl(TypeRef(struct.name), "d"),
      ),
      Block(
        VarDecl(TypeRef(typeNames.strType), "res", StrVal("")),
      ) ++Block(
        writes*
      ) ++ Block(
        Return(Var(SymbolRef("res")))
      ),
      Seq(ComAnn(s"Saves the structure ${struct.name} to a string"), StdAnn())
    )
}
