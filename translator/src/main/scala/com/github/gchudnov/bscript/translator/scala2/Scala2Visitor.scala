package com.github.gchudnov.bscript.translator.scala2

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.translator.TranslateType
import com.github.gchudnov.bscript.translator.scala2.Scala2Visitor.Scala2State
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.util.{ ShowOps, Transform }

/**
 * A visitor that translates AST to Scala 2.13 code
 *
 * NOTE: here in declarations we're using mutable data types
 *
 * NOTE: not all ASTs can be convertible to Scala. Some of them can produce ill-formed code.
 */
final class Scala2Visitor(toTargetType: TranslateType, init: ScalaInitializer) extends TreeVisitor[Scala2State, Scala2State]:
  import Scala2Visitor.*

  override def visit(s: Scala2State, n: Init): Either[Throwable, Scala2State] =
    for lines <- init.init(n.iType)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: UnaryMinus): Either[Throwable, Scala2State] =
    for
      expr <- n.expr.visit(s, this).map(_.lines)
      lines = prepend("-", rwrapMl(expr))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Add): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" + ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Sub): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" - ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Mul): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" * ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Div): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" / ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Mod): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" % ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Less): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" < ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: LessEqual): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" <= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Greater): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" > ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: GreaterEqual): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" >= ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Equal): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" == ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: NotEqual): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" != ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Not): Either[Throwable, Scala2State] =
    for
      expr <- n.visit(s, this).map(_.lines)
      lines = prepend("!", rwrapMl(expr))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: And): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" && ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Or): Either[Throwable, Scala2State] =
    for
      lhsLines <- n.lhs.visit(s, this).map(_.lines)
      rhsLines <- n.rhs.visit(s, this).map(_.lines)
      lines     = rwrap(join(" || ", rwrapMl(lhsLines), rwrapMl(rhsLines)))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Assign): Either[Throwable, Scala2State] =
    for
      id   <- n.id.visit(s, this).map(_.lines)
      expr <- n.expr.visit(s, this).map(_.lines)
      lines = joinCR(" = ", rwrapMl(id), expr)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: NothingVal): Either[Throwable, Scala2State] =
    for
      value <- Right("???")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: VoidVal): Either[Throwable, Scala2State] =
    for
      value <- Right("()")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: BoolVal): Either[Throwable, Scala2State] =
    for
      value <- Right(
                 if n.value then toTargetType.boolTrue
                 else toTargetType.boolFalse
               )
      lines = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: IntVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: LongVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"${n.value.toString}L")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: FloatVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"${n.value.toString}f")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: DoubleVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: DecimalVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"${n.value.toString}")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: StrVal): Either[Throwable, Scala2State] =
    for
      value <- Right(s"\"${n.value}\"")
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: DateVal): Either[Throwable, Scala2State] = for
    value <- Right(s"""LocalDate.parse("${n.value.toString}")""")
    lines  = Vector(value)
  yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: DateTimeVal): Either[Throwable, Scala2State] = for
    value <- Right(s"""OffsetDateTime.parse("${n.value.toString}", DateTimeFormatter.ISO_OFFSET_DATE_TIME)""")
    lines  = Vector(value)
  yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Vec): Either[Throwable, Scala2State] =
    for
      elems <- Transform.sequence(n.elements.map(_.visit(s, this).map(_.lines).map(rwrapMl)))
      lines  = wrap("List(", ")", joinAll(", ", elems))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Var): Either[Throwable, Scala2State] =
    for
      value <- Right(n.symbol.name)
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: ArgDecl): Either[Throwable, Scala2State] =
    for
      name     <- Right(n.name)
      typeName <- toTargetType.toLangTypeName(n.aType)
      value     = s"${name}: ${typeName}"
      lines     = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: VarDecl): Either[Throwable, Scala2State] =
    for
      name     <- Right(n.name)
      typeName <- toTargetType.toLangTypeName(n.vType)
      expr     <- n.expr.visit(s, this).map(_.lines)
      nameValue = if typeName.nonEmpty then s"var ${name}: ${typeName}" else s"var ${name}"
      lines     = joinCR(" = ", Vector(nameValue), expr)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: FieldDecl): Either[Throwable, Scala2State] =
    for
      name     <- Right(n.name)
      typeName <- toTargetType.toLangTypeName(n.fType)
      value     = s"var ${name}: ${typeName}"
      lines     = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: MethodDecl): Either[Throwable, Scala2State] =
    for
      args    <- Transform.sequence(n.params.map(_.visit(s, this).map(_.lines).map(rwrapMl)))
      args0    = if args.isEmpty then Seq(Seq("")) else args
      retType <- toTargetType.toLangTypeName(n.retType)
      body    <- n.body.visit(s, this).map(_.lines)
      anns     = n.annotations.map(_.value)
      wAnns    = if anns.nonEmpty then wrap("/**", " */", wrapEmpty(padLines(" * ", anns))) else Seq.empty[String]
      header   = wrap(s"def ${n.name}(", s"): ${retType}", joinAll(", ", args0))
      lines    = wAnns ++ joinCR(" = ", header, body)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: StructDecl): Either[Throwable, Scala2State] =
    for
      fields <- Transform.sequence(n.fields.map(_.visit(s, this).map(_.lines)))
      lines   = wrap(s"final case class ${n.name}(", ")", wrapEmpty(tabLines(1, joinVAll(",", fields))))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Block): Either[Throwable, Scala2State] =
    for
      stmts <- Transform.sequence(n.statements.map(_.visit(s, this).map(_.lines)))
      lines  = if stmts.nonEmpty then wrap("{", "}", wrapEmpty(tabLines(1, joinVAll("", stmts)))) else Seq("{}")
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Call): Either[Throwable, Scala2State] =
    for
      args <- Transform.sequence(n.args.map(_.visit(s, this).map(_.lines)))
      lines = wrap(s"${n.id.name}", "", rwrapIfNonWrapped(tabTail(1, joinAll(", ", args))))
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: If): Either[Throwable, Scala2State] =
    for
      cond1 <- n.cond.visit(s, this).map(_.lines)
      then1 <- n.then1.visit(s, this).map(_.lines)
      else1 <- Transform.sequence(n.else1.map(_.visit(s, this))).map(_.map(_.lines))

      cond2        = wrap("if ", "", rwrapIfNonWrapped(cond1))
      condThen     = joinCR(" ", cond2, then1)
      condThenElse = else1.map(else2 => joinCR(" else ", condThen, else2)).getOrElse(condThen)

      lines = condThenElse
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: Access): Either[Throwable, Scala2State] =
    for
      value <- Right(n.path)
      lines  = Vector(value)
    yield Scala2State(lines = lines)

  override def visit(s: Scala2State, n: CompiledExpr): Either[Throwable, Scala2State] =
    for lines <- n.callback(s).map(_.asInstanceOf[Scala2State]).map(_.lines)
    yield Scala2State(lines = lines)

object Scala2Visitor:

  def make(typeNames: TypeNames, meta: Meta): Scala2Visitor =
    val scalaTypeNames = new ScalaTypeNames(typeNames)
    val init           = new ScalaInitializer(typeNames, meta)
    new Scala2Visitor(scalaTypeNames, init)

  final case class Scala2State(lines: Seq[String]):
    def show(): String =
      ShowOps.join(lines)

  object Scala2State:
    def make(): Scala2State =
      new Scala2State(lines = Vector.empty[String])

  def wrapProgram(body: String): String =
    s"""
       |import java.time.OffsetDateTime
       |import java.time.LocalDate
       |
       |object TranslatedProgram {
       |
       |${body}
       |
       |}
       |""".stripMargin

  private def padLines(p: String, lines: Seq[String]): Seq[String] =
    ShowOps.padLines(p, lines)

  private def tabLines(depth: Int, lines: Seq[String]): Seq[String] =
    ShowOps.tabLines(depth, lines)

  private def tabTail(depth: Int, lines: Seq[String]): Seq[String] =
    ShowOps.tabTail(depth, lines)

  private def join(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    ShowOps.join(sep, lhs, rhs)

  private def joinAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    ShowOps.joinAll(sep, linesLines)

  private def joinVAll(sep: String, linesLines: Seq[Seq[String]]): Seq[String] =
    ShowOps.joinVAll(sep, linesLines)

  private def joinCR(sep: String, lhs: Seq[String], rhs: Seq[String]): Seq[String] =
    ShowOps.joinCR(sep, lhs, rhs)

  private def prepend(start: String, lines: Seq[String]): Seq[String] =
    ShowOps.prepend(start, lines)

  private def wrap(start: String, end: String, lines: Seq[String]): Seq[String] =
    ShowOps.wrap(start, end, lines)

  private def wrapEmpty(lines: Seq[String]): Seq[String] =
    ShowOps.wrapEmpty(lines)

  private def rwrap(lines: Seq[String]): Seq[String] =
    ShowOps.wrap("(", ")", lines)

  private def rwrapIfNonWrapped(lines: Seq[String]): Seq[String] =
    ShowOps.wrapIfNonWrapped("(", ")", lines)

  private def rwrapMl(lines: Seq[String]): Seq[String] =
    ShowOps.wrapIfMultiline("(", ")", lines)
