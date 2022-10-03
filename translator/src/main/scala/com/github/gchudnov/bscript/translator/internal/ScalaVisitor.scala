package com.github.gchudnov.bscript.translator.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.translator.internal.ScalaState
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{ Casting, LineOps, Transform }

/**
 * A visitor that translates AST to Scala 2.13 / 3 code
 *
 * NOTE: here in declarations we're using mutable data types
 *
 * NOTE: not all ASTs can be convertible to Scala. Some of them can produce ill-formed code.
 */
private[translator] final class ScalaVisitor(laws: TranslateLaws) extends TreeVisitor[ScalaState, ScalaState]:
  import Casting.*
  import ScalaVisitor.*

  override def visit(s: ScalaState, n: Init): Either[Throwable, ScalaState] =
    for lines <- laws.initializer.init(n.iType)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: UnaryMinus): Either[Throwable, ScalaState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("-", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: ScalaState, n: Add): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" + ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Sub): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" - ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Mul): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" * ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Div): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" / ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Mod): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" % ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Less): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" < ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: LessEqual): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" <= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Greater): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" > ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: GreaterEqual): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" >= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Equal): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" == ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: NotEqual): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" != ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Not): Either[Throwable, ScalaState] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("!", rwrapMl(exprLines))
    yield es.withLines(lines)

  override def visit(s: ScalaState, n: And): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" && ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Or): Either[Throwable, ScalaState] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" || ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.withLines(lines)

  override def visit(s: ScalaState, n: Assign): Either[Throwable, ScalaState] =
    for
      ids      <- n.id.visit(s, this)
      idLines   = ids.lines
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = joinCR(" = ", rwrapMl(idLines), exprLines)
    yield es.withLines(lines)

  override def visit(s: ScalaState, n: NothingVal): Either[Throwable, ScalaState] =
    for
      value <- Right("???")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: VoidVal): Either[Throwable, ScalaState] =
    for
      value <- Right("()")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: BoolVal): Either[Throwable, ScalaState] =
    for
      value <- Right(
                 if n.value then laws.typeConverter.trueValue
                 else laws.typeConverter.falseValue
               )
      lines = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: IntVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: LongVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"${n.value.toString}L")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: FloatVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"${n.value.toString}f")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: DoubleVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: DecimalVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: StrVal): Either[Throwable, ScalaState] =
    for
      value <- Right(s"\"${n.value}\"")
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: DateVal): Either[Throwable, ScalaState] = for
    value  <- Right(s"""LocalDate.parse("${n.value.toString}")""")
    lines   = Vector(value)
    imports = Set("java.time.LocalDate")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: ScalaState, n: DateTimeVal): Either[Throwable, ScalaState] = for
    value  <- Right(s"""OffsetDateTime.parse("${n.value.toString}", DateTimeFormatter.ISO_OFFSET_DATE_TIME)""")
    lines   = Vector(value)
    imports = Set("java.time.OffsetDateTime", "java.time.format.DateTimeFormatter")
  yield s.withLines(lines).withImports(s.imports ++ imports)

  override def visit(s: ScalaState, n: StructVal): Either[Throwable, ScalaState] =
    for
      sStruct <- n.sType.asSStruct
      sFields  = s.meta.symbolsFor(sStruct)
      ms <- sFields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, sField) =>
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

  override def visit(s: ScalaState, n: Vec): Either[Throwable, ScalaState] =
    for
      es <- n.elements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, e) =>
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

  override def visit(s: ScalaState, n: Var): Either[Throwable, ScalaState] =
    for
      value <- Right(n.symbol.name)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: ArgDecl): Either[Throwable, ScalaState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.aType)
      value     = s"${name}: ${typeName}"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: VarDecl): Either[Throwable, ScalaState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.vType)
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      nameValue = if typeName.nonEmpty then s"var ${name}: ${typeName}" else s"var ${name}"
      lines     = joinCR(" = ", Vector(nameValue), exprLines)
    yield es.withLines(lines)

  override def visit(s: ScalaState, n: FieldDecl): Either[Throwable, ScalaState] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.fType)
      value     = s"var ${name}: ${typeName}"
      lines     = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: MethodDecl): Either[Throwable, ScalaState] =
    for
      as <- n.params.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, e) =>
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
      header    = wrap(s"def ${n.name}(", s"): ${retType}", argLines)
      lines     = comment ++ joinCR(" = ", header, bodyLines)
    yield bs.withLines(lines)

  override def visit(s: ScalaState, n: StructDecl): Either[Throwable, ScalaState] =
    for
      fs <- n.fields.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll(",", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.withLines(lines)
            }
      fieldLines = fs.lines
      lines      = wrap(s"final case class ${n.name}(", ")", wrapEmpty(tabLines(1, fieldLines)))
    yield fs.withLines(lines)

  override def visit(s: ScalaState, n: Block): Either[Throwable, ScalaState] =
    for
      ss <- n.statements.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, e) =>
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

  override def visit(s: ScalaState, n: Call): Either[Throwable, ScalaState] =
    println(("n.args", n.args))
    for
      as <- n.args.foldLeft(Right(s.withLines(Seq.empty[String])): Either[Throwable, ScalaState]) { case (acc, e) =>
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

  override def visit(s: ScalaState, n: If): Either[Throwable, ScalaState] =
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

  override def visit(s: ScalaState, n: Access): Either[Throwable, ScalaState] =
    for
      value <- Right(n.path)
      lines  = Vector(value)
    yield s.withLines(lines)

  override def visit(s: ScalaState, n: CompiledExpr): Either[Throwable, ScalaState] =
    for
      cs           <- n.callback(s).map(_.asInstanceOf[ScalaState])
      callbackLines = cs.lines
      lines         = callbackLines
    yield cs.withLines(lines)

private[translator] object ScalaVisitor:

  def make(laws: TranslateLaws): ScalaVisitor =
    new ScalaVisitor(laws)

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
