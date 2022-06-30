package com.github.gchudnov.bscript.translator.internal.scala3

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.translator.internal.scala3.Scala3State
import com.github.gchudnov.bscript.translator.TranslateLaws
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.builder.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{ Casting, LineOps, Transform }

/**
 * A visitor that translates AST to Scala 2.13 code
 *
 * NOTE: here in declarations we're using mutable data types
 *
 * NOTE: not all ASTs can be convertible to Scala. Some of them can produce ill-formed code.
 */
private[translator] final class Scala3Visitor(laws: TranslateLaws) extends TreeVisitor[Scala3State, Scala3State]:
  import Casting.*
  import Scala3Visitor.*

  override def visit(s: Scala3State, n: Init): Either[Throwable, Scala3State] =
    for lines <- laws.initializer.init(n.iType)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: UnaryMinus): Either[Throwable, Scala3State] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("-", rwrapMl(exprLines))
    yield es.copy(lines = lines)

  override def visit(s: Scala3State, n: Add): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" + ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Sub): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" - ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Mul): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" * ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Div): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" / ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Mod): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" % ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Less): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" < ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: LessEqual): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" <= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Greater): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" > ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: GreaterEqual): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" >= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Equal): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" == ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: NotEqual): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" != ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Not): Either[Throwable, Scala3State] =
    for
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = prepend("!", rwrapMl(exprLines))
    yield es.copy(lines = lines)

  override def visit(s: Scala3State, n: And): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" && ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Or): Either[Throwable, Scala3State] =
    for
      ls      <- n.lhs.visit(s, this)
      lhsLines = ls.lines
      rs      <- n.rhs.visit(ls, this)
      rhsLines = rs.lines
      lines    = rwrap(join(" || ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield rs.copy(lines = lines)

  override def visit(s: Scala3State, n: Assign): Either[Throwable, Scala3State] =
    for
      ids      <- n.id.visit(s, this)
      idLines   = ids.lines
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      lines     = joinCR(" = ", rwrapMl(idLines), exprLines)
    yield es.copy(lines = lines)

  override def visit(s: Scala3State, n: NothingVal): Either[Throwable, Scala3State] =
    for
      value <- Right("???")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: VoidVal): Either[Throwable, Scala3State] =
    for
      value <- Right("()")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: BoolVal): Either[Throwable, Scala3State] =
    for
      value <- Right(
                 if n.value then laws.typeConverter.trueValue
                 else laws.typeConverter.falseValue
               )
      lines = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: IntVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: LongVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"${n.value.toString}L")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: FloatVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"${n.value.toString}f")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: DoubleVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: DecimalVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: StrVal): Either[Throwable, Scala3State] =
    for
      value <- Right(s"\"${n.value}\"")
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: DateVal): Either[Throwable, Scala3State] = for
    value  <- Right(s"""LocalDate.parse("${n.value.toString}")""")
    lines   = Vector(value)
    imports = Set("java.time.LocalDate")
  yield s.copy(lines = lines, s.imports ++ imports)

  override def visit(s: Scala3State, n: DateTimeVal): Either[Throwable, Scala3State] = for
    value  <- Right(s"""OffsetDateTime.parse("${n.value.toString}", DateTimeFormatter.ISO_OFFSET_DATE_TIME)""")
    lines   = Vector(value)
    imports = Set("java.time.OffsetDateTime", "java.time.format.DateTimeFormatter")
  yield s.copy(lines = lines, s.imports ++ imports)

  override def visit(s: Scala3State, n: StructVal): Either[Throwable, Scala3State] =
    for
      sStruct <- n.sType.asSStruct
      sFields  = s.meta.symbolsFor(sStruct)
      ms <- sFields.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, sField) =>
              acc match
                case Left(e) => Left(e)
                case Right(si) =>
                  val expr = n.value(sField.name)
                  for
                    sn   <- expr.visit(si, this)
                    lines = LineOps.joinCR(" = ", Seq(sField.name), LineOps.tabTail(1, sn.lines))
                  yield sn.copy(lines = LineOps.joinVAll(",", Seq(si.lines, lines)))
            }
      fields = ms.lines
      lines  = LineOps.wrap(s"${n.sType.name}(", ")", LineOps.wrapEmpty(LineOps.tabLines(1, fields)))
    yield ms.copy(lines = lines)

  override def visit(s: Scala3State, n: Vec): Either[Throwable, Scala3State] =
    for
      es <- n.elements.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.copy(lines = lines)
            }
      elementLines = es.lines
      lines        = wrap("List(", ")", elementLines)
    yield es.copy(lines = lines)

  override def visit(s: Scala3State, n: Var): Either[Throwable, Scala3State] =
    for
      value <- Right(n.symbol.name)
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: ArgDecl): Either[Throwable, Scala3State] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.aType)
      value     = s"${name}: ${typeName}"
      lines     = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: VarDecl): Either[Throwable, Scala3State] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.vType)
      es       <- n.expr.visit(s, this)
      exprLines = es.lines
      nameValue = if typeName.nonEmpty then s"var ${name}: ${typeName}" else s"var ${name}"
      lines     = joinCR(" = ", Vector(nameValue), exprLines)
    yield es.copy(lines = lines)

  override def visit(s: Scala3State, n: FieldDecl): Either[Throwable, Scala3State] =
    for
      name     <- Right(n.name)
      typeName <- laws.typeConverter.toTypeName(n.fType)
      value     = s"var ${name}: ${typeName}"
      lines     = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: MethodDecl): Either[Throwable, Scala3State] =
    for
      as <- n.params.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.copy(lines = lines)
            }
      argLines  = if as.lines.isEmpty then Seq("") else as.lines
      retType  <- laws.typeConverter.toTypeName(n.retType)
      bs       <- n.body.visit(as, this)
      bodyLines = bs.lines
      anns      = n.annotations.map(_.value)
      comment   = if anns.nonEmpty then wrap("/**", " */", wrapEmpty(padLines(" * ", anns))) else Seq.empty[String]
      header    = wrap(s"def ${n.name}(", s"): ${retType}", argLines)
      lines     = comment ++ joinCR(" = ", header, bodyLines)
    yield bs.copy(lines = lines)

  override def visit(s: Scala3State, n: StructDecl): Either[Throwable, Scala3State] =
    for
      fs <- n.fields.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll(",", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.copy(lines = lines)
            }
      fieldLines = fs.lines
      lines      = wrap(s"final case class ${n.name}(", ")", wrapEmpty(tabLines(1, fieldLines)))
    yield fs.copy(lines = lines)

  override def visit(s: Scala3State, n: Block): Either[Throwable, Scala3State] =
    for
      ss <- n.statements.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinVAll("", Seq(si.lines, sn.lines))
                  yield sn.copy(lines = lines)
            }
      stmtLines = ss.lines
      lines     = if stmtLines.nonEmpty then wrap("{", "}", wrapEmpty(tabLines(1, stmtLines))) else Seq("{}")
    yield ss.copy(lines = lines)

  override def visit(s: Scala3State, n: Call): Either[Throwable, Scala3State] =
    for
      as <- n.args.foldLeft(Right(s.copy(lines = Seq.empty[String])): Either[Throwable, Scala3State]) { case (acc, e) =>
              acc match
                case Left(t) => Left(t)
                case Right(si) =>
                  for
                    sn   <- e.visit(si, this)
                    lines = joinAll(", ", Seq(si.lines, rwrapMl(sn.lines)))
                  yield sn.copy(lines = lines)
            }
      argLines = if as.lines.isEmpty then Seq("") else as.lines
      lines    = wrap(s"${n.id.name}", "", rwrapIfNonWrapped(tabTail(1, argLines)))
    yield as.copy(lines = lines)

  override def visit(s: Scala3State, n: If): Either[Throwable, Scala3State] =
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
    yield ss.copy(lines = lines)

  override def visit(s: Scala3State, n: Access): Either[Throwable, Scala3State] =
    for
      value <- Right(n.path)
      lines  = Vector(value)
    yield s.copy(lines = lines)

  override def visit(s: Scala3State, n: CompiledExpr): Either[Throwable, Scala3State] =
    for
      cs           <- n.callback(s).map(_.asInstanceOf[Scala3State])
      callbackLines = cs.lines
      lines         = callbackLines
    yield cs.copy(lines = lines)

private[translator] object Scala3Visitor:

  def make(laws: TranslateLaws): Scala3Visitor =
    new Scala3Visitor(laws)

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
