package com.github.gchudnov.bscript.translator.into.asm

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{DeclType, Type, VectorType}
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{Casting, LineOps, Transform}
import com.github.gchudnov.bscript.translator.TranslateLaws

import scala.collection.immutable.Seq

/**
 * A visitor that translates AST to C code
 *
 * NOTE: here in declarations we're using mutable data types
 *
 * NOTE: not all ASTs can be convertible to Scala. Some of them can produce ill-formed code.
 */
final class AsmVisitor(laws: TranslateLaws) extends TreeVisitor[AsmState, AsmState]:
  import AsmVisitor.*
  import Casting.*

  override def visit(s: AsmState, n: Init): Either[Throwable, AsmState] =
    for lines <- laws.initializer.init(n.iType)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: UnaryMinus): Either[Throwable, AsmState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("-", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: AsmState, n: Add): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" + ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Sub): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" - ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Mul): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" * ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Div): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" / ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Mod): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" % ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Less): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" < ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: LessEqual): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" <= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Greater): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" > ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: GreaterEqual): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" >= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Equal): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" == ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: NotEqual): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" != ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Not): Either[Throwable, AsmState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("!", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: AsmState, n: Return): Either[Throwable, AsmState] =
    for
      es <- n.expr.visit(s, this)
      exprLines = es.lines
      lines = append(";", prepend("return ", rwrapMl(exprLines)))
    yield es.withLines(lines)

  override def visit(s: AsmState, n: And): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" && ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Or): Either[Throwable, AsmState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" || ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: AsmState, n: Assign): Either[Throwable, AsmState] =
    for
      ids      <- n.id.visit(s, this)
      idLines   = ids.lines
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      exprLines1 = replaceNA(n.id.evalType, exprLines)
      lines     = joinCR(" = ", rwrapMl(idLines), exprLines1)
    yield es.withLines(lines)

  override def visit(s: AsmState, n: NothingVal): Either[Throwable, AsmState] =
    for
      value <- Right("<NULL>")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: VoidVal): Either[Throwable, AsmState] =
    for
      value <- Right("<NULL>")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: BoolVal): Either[Throwable, AsmState] =
    for
      value <- Right(
                 if n.value then laws.typeConverter.trueValue
                 else laws.typeConverter.falseValue
               )
      lines = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: IntVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: LongVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: FloatVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: DoubleVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: DecimalVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: StrVal): Either[Throwable, AsmState] =
    for
      value <- Right(s"\"${n.value}\"")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: DateVal): Either[Throwable, AsmState] = for
    value  <- Right(s"""Date.parse("${n.value.toString}")""")
    lines   = Vector(value)
    imports = Set("""{ Date} from "date";""")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: AsmState, n: DateTimeVal): Either[Throwable, AsmState] = for
    value  <- Right(s"""Date.parse("${n.value.toString}")""")
    lines   = Vector(value)
    imports = Set("""{ Date} from "date";""")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: AsmState, n: StructVal): Either[Throwable, AsmState] =
    for
      sStruct <- n.sType.asSStruct
      sFields  = s.meta.symbolsFor(sStruct)
      ms <- sFields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, sField) =>
              acc match
                case Left(e) => Left(e)
                case Right(si) =>
                  val expr = n.value(sField.name)
                  for
                    sn   <- expr.visit(si, this)
                    fieldLines = sn.lines
                    keyType <- sn.meta.structTypes(sStruct).map(_(sField.name))
                    fieldLines1 = replaceNA(keyType, fieldLines)
                    lines = LineOps.joinCR(": ", Seq(sField.name), LineOps.tabTail(1, fieldLines1))
                  yield sn.withLines(LineOps.joinVAll(",", Seq(si.lines, lines)))
            }
      fields = ms.lines
      lines  = LineOps.wrap(s"{", "}", LineOps.wrapEmpty(LineOps.tabLines(1, fields)))
    yield ms.withLines(lines)

  override def visit(s: AsmState, n: Vec): Either[Throwable, AsmState] =
    for
      es <- n.elements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      elementLines = es.lines
      lines        = if elementLines.nonEmpty then wrap("[", "]", elementLines) else Seq("[]")
    yield es.withLines(lines)

  override def visit(s: AsmState, n: Var): Either[Throwable, AsmState] =
    for
      value <- Right(n.symbol.name)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: ArgDecl): Either[Throwable, AsmState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.aType)
      value     = s"${name}: ${typeName}"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: VarDecl): Either[Throwable, AsmState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.vType)
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      exprLines1 = replaceNA(n.vType, exprLines)
      nameValue = if typeName.nonEmpty then s"let ${name}: ${typeName}" else s"let ${name}"
      lines     = joinCR(" = ", Vector(nameValue), exprLines1)
    yield es.withLines(lines)

  override def visit(s: AsmState, n: FieldDecl): Either[Throwable, AsmState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.fType)
      value     = s"${name}: ${typeName}"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: MethodDecl): Either[Throwable, AsmState] =
    for
      as <- n.params.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      argLines  = if as.lines.isEmpty then Seq("") else as.lines
      retType  <- laws.typeConverter.toTypeName(n.retType)
      bs       <- n.body.visit(as, this)
      bodyLines = bs.lines
      anns      = n.annotations.map(_.value)
      comment   = if anns.nonEmpty then wrap("/**", " */", wrapEmpty(padLines(" * ", anns))) else Seq.empty[String]
      header    = wrap(s"function ${n.name}(", s"): ${retType}", argLines)
      lines     = comment ++ joinCR(" ", header, bodyLines)
    yield bs.withLines(lines)

  override def visit(s: AsmState, n: StructDecl): Either[Throwable, AsmState] =
    for
      fs <- n.fields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll("", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      fieldLines = fs.lines
      lines      = wrap(s"class ${n.name} {", "}", wrapEmpty(tabLines(1, fieldLines)))
    yield fs.withLines(lines)

  override def visit(s: AsmState, n: Block): Either[Throwable, AsmState] =
    for
      ss <- n.statements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll("", Seq(si.lines, sn.lines))
                  yield sn.withLines(lines)
            }
      stmtLines = ss.lines
      lines     = if stmtLines.nonEmpty then wrap("{", "}", wrapEmpty(tabLines(1, stmtLines))) else Seq("{}")
    yield ss.withLines(lines)

  override def visit(s: AsmState, n: Module): Either[Throwable, AsmState] =
    for
      ss <- n.statements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
        acc match
          case Left(t) => Left(t)
          case Right(si) =>
            for
              sn   <- e.visit(si, this)
              lines = joinVAll("", Seq(si.lines, sn.lines))
            yield sn.withLines(lines)
      }
      stmtLines = ss.lines
      lines     = stmtLines
    yield ss.withLines(lines)

  override def visit(s: AsmState, n: Call): Either[Throwable, AsmState] =
    for
      as <- n.args.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, AsmState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      argLines = if as.lines.isEmpty then Seq("") else as.lines
      lines    = wrap(s"${n.id.name}", "", rwrapIfNonWrapped(tabTail(1, argLines)))
    yield as.withLines(lines)

  override def visit(s: AsmState, n: If): Either[Throwable, AsmState] =
    for
      cs       <- n.cond.visit(s, this)
      ts       <- n.then1.visit(cs, this)
      es       <- Transform.sequence(n.else1.map(_.visit(ts, this)))
      condLines = cs.lines
      thenLines = ts.lines
      elseLines = es.map(_.lines)

      cond2        = wrap("if ", "", rwrapIfNonWrapped(condLines))
      condThen     = joinCR(" ", cond2, thenLines)
      condThenElse = elseLines.map(else2 => joinCR(" else ", condThen, else2)).getOrElse(condThen)

      lines = condThenElse
      ss    = es.getOrElse(ts)
    yield ss.withLines(lines)

  override def visit(s: AsmState, n: Access): Either[Throwable, AsmState] =
    for
      value <- Right(n.path)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: AsmState, n: CompiledExpr): Either[Throwable, AsmState] =
    for
      cs           <- n.callback(s).map(_.asInstanceOf[AsmState])
      callbackLines = cs.lines
      lines         = callbackLines
    yield cs.withLines(lines)

  private def replaceNA(t: Type, lines: Seq[String]) =
    if lines.contains("<NULL>") then
      lines.map(line => line.replace("<NULL>", NAforType(t)))
    else lines

  private def NAforType(t: Type): String =
    laws.initializer.na(t).fold(t => throw t, identity)

object AsmVisitor:

  def make(laws: TranslateLaws): AsmVisitor =
    new AsmVisitor(laws)

  private def append(end: String, lines: Seq[String]): Seq[String] =
    LineOps.append(end, lines)

  private def appendIfNotExists(end: String, lines: Seq[String]): Seq[String] =
    LineOps.appendIfNotExists(end, lines)

  private def padLines(p: String, lines: Seq[String]): Seq[String] =
    LineOps.padLines(p, lines)

  private def tabLines(depth: Int, lines: Seq[String]): Seq[String] =
    LineOps.tabLines(depth, lines)

  private def tabTail(depth: Int, lines: Seq[String]): Seq[String] =
    LineOps.tabTail(depth, lines)

  private def join(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    LineOps.join(sep, lhs, rhs)

  private def joinAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    LineOps.joinAll(sep, linesLines)

  private def joinVAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    LineOps.joinVAll(sep, linesLines)

  private def joinCR(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    LineOps.joinCR(sep, lhs, rhs)

  private def prepend(start: String, lines: Seq[String]): Seq[String] =
    LineOps.prepend(start, lines)

  private def wrap(start: String, end: String, lines: Seq[String]): Seq[String] =
    LineOps.wrap(start, end, lines)

  private def wrapEmpty(lines: Seq[String]): Seq[String] =
    LineOps.wrapEmpty(lines)

  private def rwrap(lines: Seq[String]): Seq[String] =
    LineOps.wrap("(", ")", lines)

  private def rwrapIfNonWrapped(lines: Seq[String]): Seq[String] =
    LineOps.wrapIfNonWrapped("(", ")", lines)

  private def rwrapMl(lines: Seq[String]): Seq[String] =
    LineOps.wrapIfMultiline("(", ")", lines)
