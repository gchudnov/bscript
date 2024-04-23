package com.github.gchudnov.bscript.translator.internal.c

import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
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
private[translator] final class CVisitor(laws: TranslateLaws) extends TreeVisitor[CState, CState]:
  import Casting.*
  import CVisitor.*

  override def visit(s: CState, n: Init): Either[Throwable, CState] =
    for lines <- laws.initializer.init(n.iType)
    yield s.withLines(lines)

  override def visit(s: CState, n: UnaryMinus): Either[Throwable, CState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("-", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: CState, n: Add): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" + ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Sub): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" - ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Mul): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" * ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Div): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" / ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Mod): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" % ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Less): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" < ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: LessEqual): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" <= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Greater): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" > ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: GreaterEqual): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" >= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Equal): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" == ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: NotEqual): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" != ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Not): Either[Throwable, CState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("!", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: CState, n: And): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" && ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Or): Either[Throwable, CState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" || ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: CState, n: Assign): Either[Throwable, CState] =
    for
      ids      <- n.id.visit(s, this)
      idLines   = ids.lines
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = append(";", joinCR(" = ", rwrapMl(idLines), exprLines))
    yield es.withLines(lines)

  override def visit(s: CState, n: NothingVal): Either[Throwable, CState] =
    for
      value <- Right("NULL")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: VoidVal): Either[Throwable, CState] =
    for
      value <- Right("NULL")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: BoolVal): Either[Throwable, CState] =
    for
      value <- Right(
                 if n.value then laws.typeConverter.trueValue
                 else laws.typeConverter.falseValue
               )
      lines = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: IntVal): Either[Throwable, CState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: LongVal): Either[Throwable, CState] =
    for
      value <- Right(s"${n.value.toString}L")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: FloatVal): Either[Throwable, CState] =
    for
      value <- Right(s"${n.value.toString}f")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: DoubleVal): Either[Throwable, CState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: DecimalVal): Either[Throwable, CState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: StrVal): Either[Throwable, CState] =
    for
      value <- Right(s"\"${n.value}\"")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: DateVal): Either[Throwable, CState] = for
    value  <- Right(s"""LocalDate.parse("${n.value.toString}")""")
    lines   = Vector(value)
    imports = Set("java.time.LocalDate")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: CState, n: DateTimeVal): Either[Throwable, CState] = for
    value  <- Right(s"""OffsetDateTime.parse("${n.value.toString}", DateTimeFormatter.ISO_OFFSET_DATE_TIME)""")
    lines   = Vector(value)
    imports = Set("java.time.OffsetDateTime", "java.time.format.DateTimeFormatter")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: CState, n: StructVal): Either[Throwable, CState] =
    for
      sStruct <- n.sType.asSStruct
      sFields  = s.meta.symbolsFor(sStruct)
      ms <- sFields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, sField) =>
              acc match
                case Left(e) => Left(e)
                case Right(si) =>
                  val expr = n.value(sField.name)
                  for
                    sn   <- expr.visit(si, this)
                    lines = LineOps.joinCR(" = ", Seq(sField.name), LineOps.tabTail(1, sn.lines))
                  yield sn.withLines(LineOps.joinVAll(",", Seq(si.lines, lines)))
            }
      fields = ms.lines
      lines  = LineOps.wrap(s"${n.sType.name}(", ")", LineOps.wrapEmpty(LineOps.tabLines(1, fields)))
    yield ms.withLines(lines)

  override def visit(s: CState, n: Vec): Either[Throwable, CState] =
    for
      es <- n.elements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      elementLines = es.lines
      lines        = if elementLines.nonEmpty then wrap("List(", ")", elementLines) else Seq("List.empty")
    yield es.withLines(lines)

  override def visit(s: CState, n: Var): Either[Throwable, CState] =
    for
      value <- Right(n.symbol.name)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: ArgDecl): Either[Throwable, CState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.aType)
      value     = s"${typeName} ${name}"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: VarDecl): Either[Throwable, CState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.vType)
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      nameValue = if typeName.nonEmpty then s"${typeName} ${name}" else s"auto ${name}"
      lines     = append(";", joinCR(" = ", Vector(nameValue), exprLines))
    yield es.withLines(lines)

  override def visit(s: CState, n: FieldDecl): Either[Throwable, CState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.fType)
      value     = s"${typeName} ${name};"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: MethodDecl): Either[Throwable, CState] =
    for
      as <- n.params.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, e) =>
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
      header    = wrap(s"${retType} ${n.name}(", s")", argLines)
      lines     = comment ++ joinCR(" ", header, bodyLines)
    yield bs.withLines(lines)

  override def visit(s: CState, n: StructDecl): Either[Throwable, CState] =
    for
      fs <- n.fields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll("", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      fieldLines = fs.lines
      lines      = wrap(s"struct ${n.name} {", "};", wrapEmpty(tabLines(1, fieldLines)))
    yield fs.withLines(lines)

  override def visit(s: CState, n: Block): Either[Throwable, CState] =
    for
      ss <- n.statements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, e) =>
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

  override def visit(s: CState, n: Call): Either[Throwable, CState] =
    for
      as <- n.args.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, CState]) { case (acc, e) =>
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

  override def visit(s: CState, n: If): Either[Throwable, CState] =
    for
      cs       <- n.cond.visit(s, this)
      ts       <- n.then1.visit(cs, this)
      es       <- Transform.sequence(n.else1.map(_.visit(ts, this)))
      condLines = cs.lines
      thenLines = ts.lines
      elseLines = es.map(_.lines)

      cond2        = wrap("if ", "", rwrapIfNonWrapped(condLines))
      condThen     = joinCR(" then ", cond2, thenLines)
      condThenElse = elseLines.map(else2 => joinCR(" else ", condThen, else2)).getOrElse(condThen)

      lines = condThenElse
      ss    = es.getOrElse(ts)
    yield ss.withLines(lines)

  override def visit(s: CState, n: Access): Either[Throwable, CState] =
    for
      value <- Right(n.path)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: CState, n: CompiledExpr): Either[Throwable, CState] =
    for
      cs           <- n.callback(s).map(_.asInstanceOf[CState])
      callbackLines = cs.lines
      lines         = callbackLines
    yield cs.withLines(lines)

private[translator] object CVisitor:

  def make(laws: TranslateLaws): CVisitor =
    new CVisitor(laws)

  private def append(end: String, lines: Seq[String]): Seq[String] =
    LineOps.append(end, lines)

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
