package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Symbol, Type, VectorType }
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.serde.internal.Keys.*
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

private[internal] final class JSONSerializeVisitor extends TreeVisitor[Unit, JValue]:

  override def visit(s: Unit, n: Init): Either[Throwable, JValue] =
    for
      iType         <- visitType(n.iType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      jValue         = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> iType))
    yield jValue

  override def visit(s: Unit, n: UnaryMinus): Either[Throwable, JValue] =
    visitUnOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Add): Either[Throwable, JValue] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Sub): Either[Throwable, JValue] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Mul): Either[Throwable, JValue] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Div): Either[Throwable, JValue] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Mod): Either[Throwable, JValue] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Less): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: LessEqual): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Greater): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: GreaterEqual): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Equal): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: NotEqual): Either[Throwable, JValue] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Not): Either[Throwable, JValue] =
    visitUnLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: And): Either[Throwable, JValue] =
    visitLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Or): Either[Throwable, JValue] =
    visitLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: Unit, n: Assign): Either[Throwable, JValue] =
    for
      id    <- n.id.visit((), this)
      expr  <- n.expr.visit((), this)
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.expr -> expr))
    yield jValue

  override def visit(s: Unit, n: NothingVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      jValue = ((Keys.kind -> s"${kind}"))
    yield jValue

  override def visit(s: Unit, n: VoidVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      jValue = ((Keys.kind -> s"${kind}"))
    yield jValue

  override def visit(s: Unit, n: BoolVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: IntVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: LongVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: FloatVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: DoubleVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: DecimalVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: StrVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: DateVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: DateTimeVal): Either[Throwable, JValue] =
    for
      kind  <- Right(n.getClass.getSimpleName)
      value <- Right(n.value.toString)
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: StructVal): Either[Throwable, JValue] =
    for
      kind <- Right(n.getClass.getSimpleName)
      sType <- visitType(n.sType)      
      value <- n.value.foldLeft(Right(Map.empty[String, JValue]): Either[Throwable, Map[String, JValue]]) { case (acc, (k, v)) =>
                 acc match
                   case Left(e) => Left(e)
                   case Right(m) =>
                     for y <- v.visit((), this)
                     yield (m + (k -> y))
               }
      jValue = ((Keys.kind -> s"${kind}") ~ (Keys.xType -> sType) ~ (Keys.value -> value))
    yield jValue

  override def visit(s: Unit, n: Vec): Either[Throwable, JValue] =
    for
      elements    <- Transform.sequence(n.elements.map(n1 => n1.visit((), this)))
      elementType <- if n.elementType != Type.Undefined then visitType(n.elementType) else Right(JNothing)
      jValue       = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.elements -> elements) ~ (Keys.elementType -> elementType))
    yield jValue

  override def visit(s: Unit, n: Var): Either[Throwable, JValue] =
    for
      symbol <- visitSymbol(n.symbol)
      jValue  = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.symbol -> symbol))
    yield jValue

  override def visit(s: Unit, n: ArgDecl): Either[Throwable, JValue] =
    for
      aType <- visitType(n.aType)
      name  <- Right(n.name)
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> aType) ~ (Keys.name -> name))
    yield jValue

  override def visit(s: Unit, n: VarDecl): Either[Throwable, JValue] =
    for
      vType <- visitType(n.vType)
      name  <- Right(n.name)
      expr  <- n.expr.visit((), this)
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> vType) ~ (Keys.name -> name) ~ (Keys.expr -> expr))
    yield jValue

  override def visit(s: Unit, n: FieldDecl): Either[Throwable, JValue] =
    for
      fType <- visitType(n.fType)
      name  <- Right(n.name)
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> fType) ~ (Keys.name -> name))
    yield jValue

  override def visit(s: Unit, n: MethodDecl): Either[Throwable, JValue] =
    for
      retType <- visitType(n.retType)
      name    <- Right(n.name)
      params  <- Transform.sequence(n.params.map(n1 => n1.visit((), this)))
      body    <- n.body.visit((), this)
      jValue   = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType) ~ (Keys.name -> name) ~ (Keys.params -> params) ~ (Keys.body -> body))
    yield jValue

  override def visit(s: Unit, n: StructDecl): Either[Throwable, JValue] =
    for
      name   <- Right(n.name)
      fields <- Transform.sequence(n.fields.map(n1 => n1.visit((), this)))
      jValue  = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.name -> name) ~ (Keys.fields -> fields))
    yield jValue

  override def visit(s: Unit, n: Block): Either[Throwable, JValue] =
    for
      statements <- Transform.sequence(n.statements.map(n1 => n1.visit((), this)))
      jValue      = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.statements -> statements))
    yield jValue

  override def visit(s: Unit, n: Call): Either[Throwable, JValue] =
    for
      id    <- visitSymbol(n.id)
      args  <- Transform.sequence(n.args.map(n1 => n1.visit((), this)))
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.args -> args))
    yield jValue

  override def visit(s: Unit, n: If): Either[Throwable, JValue] =
    for
      cond  <- n.cond.visit((), this)
      then1 <- n.then1.visit((), this)
      else1 <- Transform.sequence(n.else1.map(_.visit((), this))).map(_.getOrElse(JNull))
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.cond -> cond) ~ (Keys.then1 -> then1) ~ (Keys.else1 -> else1))
    yield jValue

  override def visit(s: Unit, n: Access): Either[Throwable, JValue] =
    for
      a     <- n.a.visit((), this)
      b     <- n.b.visit((), this)
      jValue = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.a -> a) ~ (Keys.b -> b))
    yield jValue

  override def visit(s: Unit, n: CompiledExpr): Either[Throwable, JValue] =
    for
      retType <- visitType(n.retType)
      jValue   = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType))
    yield jValue

  private def visitBinOp(name: String, n: BinOp): Either[Throwable, JValue] =
    for
      lhs   <- n.lhs.visit((), this)
      rhs   <- n.rhs.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs))
    yield jValue

  private def visitRelOp(name: String, n: RelOp): Either[Throwable, JValue] =
    for
      lhs   <- n.lhs.visit((), this)
      rhs   <- n.rhs.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs))
    yield jValue

  private def visitRelOp(name: String, n: EqOp): Either[Throwable, JValue] =
    for
      lhs   <- n.lhs.visit((), this)
      rhs   <- n.rhs.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs))
    yield jValue

  private def visitUnOp(name: String, n: UnOp): Either[Throwable, JValue] =
    for
      expr  <- n.expr.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.expr -> expr))
    yield jValue

  private def visitUnLogicOp(name: String, n: UnLogicOp): Either[Throwable, JValue] =
    for
      expr  <- n.expr.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.expr -> expr))
    yield jValue

  private def visitLogicOp(name: String, n: LogicOp): Either[Throwable, JValue] =
    for
      lhs   <- n.lhs.visit((), this)
      rhs   <- n.rhs.visit((), this)
      jValue = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs))
    yield jValue

  private def visitSymbol(sym: Symbol): Either[Throwable, JValue] =
    for name <- Right(sym.name)
    yield JString(name)

  private def visitType(t: Type): Either[Throwable, JValue] =
    t match
      case x @ VectorType(elementType) =>
        for
          jElementType <- visitType(elementType)
          jValue        = ((Keys.kind -> x.getClass.getSimpleName) ~ (Keys.elementType -> jElementType))
        yield jValue
      case x @ DeclType(expr) =>
        for
          jExpr <- expr.visit((), this)
          jValue = ((Keys.kind -> x.getClass.getSimpleName) ~ (Keys.expr -> jExpr))
        yield jValue
      case _ =>
        Right(JString(t.name))

  private def visitOptType(ot: Option[Type]): Either[Throwable, JValue] =
    Transform.sequence(ot.map(t => visitType(t))).map(_.getOrElse(JNull))

private[internal] object JSONSerializeVisitor:

  def make(): JSONSerializeVisitor =
    new JSONSerializeVisitor()
