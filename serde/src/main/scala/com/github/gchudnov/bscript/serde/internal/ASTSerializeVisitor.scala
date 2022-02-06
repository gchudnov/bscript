package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Symbol, Type, VectorType }
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.serde.internal.ASTSerializeVisitor.ASTSerializeState
import com.github.gchudnov.bscript.serde.internal.Keys.*
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

final class ASTSerializeVisitor extends TreeVisitor[ASTSerializeState, ASTSerializeState]:

  override def visit(s: ASTSerializeState, n: Init): Either[Throwable, ASTSerializeState] =
    for
      iType         <- visitType(n.iType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> iType) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: UnaryMinus): Either[Throwable, ASTSerializeState] =
    visitUnOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Add): Either[Throwable, ASTSerializeState] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Sub): Either[Throwable, ASTSerializeState] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Mul): Either[Throwable, ASTSerializeState] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Div): Either[Throwable, ASTSerializeState] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Mod): Either[Throwable, ASTSerializeState] =
    visitBinOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Less): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: LessEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Greater): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: GreaterEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Equal): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: NotEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Not): Either[Throwable, ASTSerializeState] =
    visitUnLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: And): Either[Throwable, ASTSerializeState] =
    visitLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Or): Either[Throwable, ASTSerializeState] =
    visitLogicOp(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Assign): Either[Throwable, ASTSerializeState] =
    for
      id            <- n.id.visit(ASTSerializeState.empty, this).map(_.data)
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: NothingVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: VoidVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: BoolVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: IntVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: LongVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: FloatVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: DoubleVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: DecimalVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: StrVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: DateVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: DateTimeVal): Either[Throwable, ASTSerializeState] =
    visitConstVal(n.getClass.getSimpleName, n)

  override def visit(s: ASTSerializeState, n: Vec): Either[Throwable, ASTSerializeState] =
    for
      elements      <- Transform.sequence(n.elements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      elementType   <- visitType(n.elementType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.elements -> elements) ~ (Keys.elementType -> elementType) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Var): Either[Throwable, ASTSerializeState] =
    for
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: ArgDecl): Either[Throwable, ASTSerializeState] =
    for
      aType         <- visitType(n.aType)
      name          <- Right(n.name)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> aType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: VarDecl): Either[Throwable, ASTSerializeState] =
    for
      vType         <- visitType(n.vType)
      name          <- Right(n.name)
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> vType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: FieldDecl): Either[Throwable, ASTSerializeState] =
    for
      fType         <- visitType(n.fType)
      name          <- Right(n.name)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> fType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: MethodDecl): Either[Throwable, ASTSerializeState] =
    for
      retType       <- visitType(n.retType)
      name          <- Right(n.name)
      params        <- Transform.sequence(n.params.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      body          <- n.body.visit(ASTSerializeState.empty, this).map(_.data)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType) ~ (Keys.name -> name) ~ (Keys.params -> params) ~ (Keys.symbol -> symbol) ~ (Keys.body -> body) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: StructDecl): Either[Throwable, ASTSerializeState] =
    for
      name          <- Right(n.name)
      fields        <- Transform.sequence(n.fields.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.name -> name) ~ (Keys.fields -> fields) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Block): Either[Throwable, ASTSerializeState] =
    for
      statements    <- Transform.sequence(n.statements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.name -> name) ~ (Keys.statements -> statements) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Call): Either[Throwable, ASTSerializeState] =
    for
      id            <- visitSymbol(n.id)
      args          <- Transform.sequence(n.args.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.args -> args) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: If): Either[Throwable, ASTSerializeState] =
    for
      cond          <- n.cond.visit(ASTSerializeState.empty, this).map(_.data)
      then1         <- n.then1.visit(ASTSerializeState.empty, this).map(_.data)
      else1         <- Transform.sequence(n.else1.map(_.visit(ASTSerializeState.empty, this).map(_.data))).map(_.getOrElse(JNull))
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 =
        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.cond -> cond) ~ (Keys.then1 -> then1) ~ (Keys.else1 -> else1) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Access): Either[Throwable, ASTSerializeState] =
    for
      a             <- n.a.visit(ASTSerializeState.empty, this).map(_.data)
      b             <- n.b.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.a -> a) ~ (Keys.b -> b) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: CompiledExpr): Either[Throwable, ASTSerializeState] =
    for
      retType       <- visitType(n.retType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitBinOp(name: String, n: BinOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitRelOp(name: String, n: RelOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitRelOp(name: String, n: EqOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitUnOp(name: String, n: UnOp): Either[Throwable, ASTSerializeState] =
    for
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitUnLogicOp(name: String, n: UnLogicOp): Either[Throwable, ASTSerializeState] =
    for
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitLogicOp(name: String, n: LogicOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.lhs -> lhs) ~ (Keys.rhs -> rhs) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitConstVal(name: String, n: ConstVal): Either[Throwable, ASTSerializeState] =
    for
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1             = ((Keys.kind -> s"${name}") ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
    yield ASTSerializeState(data = s1)

  private def visitSymbol(sym: Symbol): Either[Throwable, JValue] =
    for
      name <- Right(sym.name)
      s1    = ((Keys.kind -> "Symbol") ~ (Keys.name -> name))
    yield s1

  private def visitType(t: Type): Either[Throwable, JValue] =
    t match
      case VectorType(elementType) =>
        for
          resolvedElementType <- visitType(elementType)
          s1                   = ((Keys.kind -> "VectorType") ~ (Keys.elementType -> resolvedElementType))
        yield s1
      case DeclType(expr) =>
        for
          resolvedExpr <- expr.visit(ASTSerializeState.empty, this).map(_.data)
          s1            = ((Keys.kind -> "DeclType") ~ (Keys.expr -> resolvedExpr))
        yield s1
      case _ =>
        Right(JString(t.name))

  private def visitOptType(ot: Option[Type]): Either[Throwable, JValue] =
    Transform.sequence(ot.map(t => visitType(t))).map(_.getOrElse(JNull))

object ASTSerializeVisitor:

  def make(): ASTSerializeVisitor =
    new ASTSerializeVisitor()

  final case class ASTSerializeState(data: JValue)

  object ASTSerializeState:
    val empty: ASTSerializeState = ASTSerializeState(JNothing)

    def make(): ASTSerializeState =
      ASTSerializeState.empty
