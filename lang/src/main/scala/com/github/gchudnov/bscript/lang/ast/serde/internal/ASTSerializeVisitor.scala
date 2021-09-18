package com.github.gchudnov.bscript.lang.ast.serde.internal

import com.github.gchudnov.bscript.lang.ast.serde.internal.ASTSerializeVisitor.ASTSerializeState
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Symbol, Type, VectorType }
import com.github.gchudnov.bscript.lang.util.{ ShowOps, Transform }

final class ASTSerializeVisitor(meta: Meta) extends TreeVisitor[ASTSerializeState, ASTSerializeState]:

  override def visit(s: ASTSerializeState, n: Init): Either[Throwable, ASTSerializeState] =
    for
      iType         <- visitType(n.iType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(s"""{
                     |  "kind": "Init",
                     |  "iType": ${iType}
                     |  "evalType": ${evalType},
                     |  "promoteToType": ${promoteToType}
                     |}
                     |""".stripMargin)
    yield ASTSerializeState(data = s1)

  // TODO: use JSON library that can work with scala 3; TODO: add serde library, do not have this code in 'lang'

  override def visit(s: ASTSerializeState, n: UnaryMinus): Either[Throwable, ASTSerializeState] =
    visitUnOp("UnaryMinus", n)

  override def visit(s: ASTSerializeState, n: Add): Either[Throwable, ASTSerializeState] =
    visitBinOp("Add", n)

  override def visit(s: ASTSerializeState, n: Sub): Either[Throwable, ASTSerializeState] =
    visitBinOp("Sub", n)

  override def visit(s: ASTSerializeState, n: Mul): Either[Throwable, ASTSerializeState] =
    visitBinOp("Mul", n)

  override def visit(s: ASTSerializeState, n: Div): Either[Throwable, ASTSerializeState] =
    visitBinOp("Div", n)

  override def visit(s: ASTSerializeState, n: Mod): Either[Throwable, ASTSerializeState] =
    visitBinOp("Mod", n)

  override def visit(s: ASTSerializeState, n: Less): Either[Throwable, ASTSerializeState] =
    visitRelOp("Less", n)

  override def visit(s: ASTSerializeState, n: LessEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp("LessEqual", n)

  override def visit(s: ASTSerializeState, n: Greater): Either[Throwable, ASTSerializeState] =
    visitRelOp("Greater", n)

  override def visit(s: ASTSerializeState, n: GreaterEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp("GreaterEqual", n)

  override def visit(s: ASTSerializeState, n: Equal): Either[Throwable, ASTSerializeState] =
    visitRelOp("Equal", n)

  override def visit(s: ASTSerializeState, n: NotEqual): Either[Throwable, ASTSerializeState] =
    visitRelOp("NotEqual", n)

  override def visit(s: ASTSerializeState, n: Not): Either[Throwable, ASTSerializeState] =
    visitUnLogicOp("Not", n)

  override def visit(s: ASTSerializeState, n: And): Either[Throwable, ASTSerializeState] =
    visitLogicOp("And", n)

  override def visit(s: ASTSerializeState, n: Or): Either[Throwable, ASTSerializeState] =
    visitLogicOp("Or", n)

  override def visit(s: ASTSerializeState, n: Assign): Either[Throwable, ASTSerializeState] =
    for
      id            <- n.id.visit(ASTSerializeState.empty, this).map(_.data)
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Assign",
                 |  "id": ${id},
                 |  "expr": ${expr},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: NothingVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("NothingVal", n)

  override def visit(s: ASTSerializeState, n: VoidVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("VoidVal", n)

  override def visit(s: ASTSerializeState, n: BoolVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("BoolVal", n)

  override def visit(s: ASTSerializeState, n: IntVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("IntVal", n)

  override def visit(s: ASTSerializeState, n: LongVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("LongVal", n)

  override def visit(s: ASTSerializeState, n: FloatVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("FloatVal", n)

  override def visit(s: ASTSerializeState, n: DoubleVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("DoubleVal", n)

  override def visit(s: ASTSerializeState, n: DecimalVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("DecimalVal", n)

  override def visit(s: ASTSerializeState, n: StrVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("StrVal", n)

  override def visit(s: ASTSerializeState, n: DateVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("DateVal", n)

  override def visit(s: ASTSerializeState, n: DateTimeVal): Either[Throwable, ASTSerializeState] =
    visitConstVal("DateTimeVal", n)

  override def visit(s: ASTSerializeState, n: Vec): Either[Throwable, ASTSerializeState] =
    for
      elements      <- Transform.sequence(n.elements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      elementType   <- visitType(n.elementType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Vec",
                 |  "elements": [${ShowOps.joinVAll(",", elements.map(it => ShowOps.split(it)))}],
                 |  "elementType": ${elementType},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Var): Either[Throwable, ASTSerializeState] =
    for
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Var",
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: ArgDecl): Either[Throwable, ASTSerializeState] =
    for
      aType         <- visitType(n.aType)
      name          <- Right(n.name)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "ArgDecl",
                 |  "aType": ${aType},
                 |  "name": ${name},
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: VarDecl): Either[Throwable, ASTSerializeState] =
    for
      vType         <- visitType(n.vType)
      name          <- Right(n.name)
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "VarDecl",
                 |  "vType": ${vType},
                 |  "name": ${name},
                 |  "expr": ${expr},
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: FieldDecl): Either[Throwable, ASTSerializeState] =
    for
      fType         <- visitType(n.fType)
      name          <- Right(n.name)
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "ArgDecl",
                 |  "fType": ${fType},
                 |  "name": ${name},
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
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
      s1 <- Right(
              s"""{
                 |  "kind": "MethodDecl",
                 |  "retType": ${retType},
                 |  "name": "${name}",
                 |  "params": [${ShowOps.joinVAll(",", params.map(it => ShowOps.split(it)))}],
                 |  "body": ${body},
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: StructDecl): Either[Throwable, ASTSerializeState] =
    for
      name          <- Right(n.name)
      fields        <- Transform.sequence(n.fields.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "StructDecl",
                 |  "name": "${name}",
                 |  "fields": [${ShowOps.joinVAll(",", fields.map(it => ShowOps.split(it)))}],
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Block): Either[Throwable, ASTSerializeState] =
    for
      statements    <- Transform.sequence(n.statements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      symbol        <- visitSymbol(n.symbol)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Block",
                 |  "statements": [${ShowOps.joinVAll(",", statements.map(it => ShowOps.split(it)))}],
                 |  "symbol": ${symbol},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Call): Either[Throwable, ASTSerializeState] =
    for
      id            <- visitSymbol(n.id)
      args          <- Transform.sequence(n.args.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Call",
                 |  "id": ${id},
                 |  "args": [${ShowOps.joinVAll(",", args.map(it => ShowOps.split(it)))}],
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: If): Either[Throwable, ASTSerializeState] =
    for
      cond          <- n.cond.visit(ASTSerializeState.empty, this).map(_.data)
      then1         <- n.then1.visit(ASTSerializeState.empty, this).map(_.data)
      else1         <- Transform.sequence(n.else1.map(_.visit(ASTSerializeState.empty, this).map(_.data))).map(_.getOrElse(ShowOps.CNull))
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "If",
                 |  "cond": ${cond},
                 |  "then1": ${then1},
                 |  "else1": ${else1},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: Access): Either[Throwable, ASTSerializeState] =
    for
      a             <- n.a.visit(ASTSerializeState.empty, this).map(_.data)
      b             <- n.b.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "Access",
                 |  "a": ${a},
                 |  "b": ${b},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  override def visit(s: ASTSerializeState, n: CompiledExpr): Either[Throwable, ASTSerializeState] =
    for
      retType       <- visitType(n.retType)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "CompiledExpr",
                 |  "callback": "N/A",
                 |  "retType": ${retType},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitBinOp(name: String, n: BinOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "${name}",
                 |  "lhs": ${lhs},
                 |  "rhs": ${rhs},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitRelOp(name: String, n: RelOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "${name}",
                 |  "lhs": ${lhs},
                 |  "rhs": ${rhs},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitRelOp(name: String, n: EqOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "${name}",
                 |  "lhs": ${lhs},
                 |  "rhs": ${rhs},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitUnOp(name: String, n: UnOp): Either[Throwable, ASTSerializeState] =
    for
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(s"""{
                     |  "kind": "${name}",
                     |  "expr": ${expr},
                     |  "evalType": ${evalType},
                     |  "promoteToType": ${promoteToType}
                     |}
                     |""".stripMargin)
    yield ASTSerializeState(data = s1)

  private def visitUnLogicOp(name: String, n: UnLogicOp): Either[Throwable, ASTSerializeState] =
    for
      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(s"""{
                     |  "kind": "${name}",
                     |  "expr": ${expr},
                     |  "evalType": ${evalType},
                     |  "promoteToType": ${promoteToType}
                     |}
                     |""".stripMargin)
    yield ASTSerializeState(data = s1)

  private def visitLogicOp(name: String, n: LogicOp): Either[Throwable, ASTSerializeState] =
    for
      lhs           <- n.lhs.visit(ASTSerializeState.empty, this).map(_.data)
      rhs           <- n.rhs.visit(ASTSerializeState.empty, this).map(_.data)
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "${name}",
                 |  "lhs": ${lhs},
                 |  "rhs": ${rhs},
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitConstVal(name: String, n: ConstVal): Either[Throwable, ASTSerializeState] =
    for
      evalType      <- visitType(n.evalType)
      promoteToType <- visitOptType(n.promoteToType)
      s1 <- Right(
              s"""{
                 |  "kind": "${name}",
                 |  "evalType": ${evalType},
                 |  "promoteToType": ${promoteToType}
                 |}
                 |""".stripMargin
            )
    yield ASTSerializeState(data = s1)

  private def visitSymbol(sym: Symbol): Either[Throwable, String] =
    for
      id   <- meta.id(sym)
      name <- Right(sym.name)
      s1 <- Right(
              s"""{
                 |  "kind": "Symbol",
                 |  "id": "${id}",
                 |  "name": "${name}"
                 |}
                 |""".stripMargin
            )
    yield s1

  private def visitType(t: Type): Either[Throwable, String] =
    t match
      case VectorType(elementType) =>
        for
          resolvedElementType <- visitType(elementType)
          s1 <- Right(
                  s"""{
                     |  "kind": "VectorType",
                     |  "elementType": ${resolvedElementType}
                     |}
                     |""".stripMargin
                )
        yield s1
      case DeclType(expr) =>
        for
          resolvedExpr <- expr.visit(ASTSerializeState.empty, this).map(_.data)
          s1 <- Right(
                  s"""{
                     |  "kind": "DeclType",
                     |  "expr": ${resolvedExpr}
                     |}
                     |""".stripMargin
                )
        yield s1
      case _ =>
        Right(s""""${t.name}"""")

  private def visitOptType(ot: Option[Type]): Either[Throwable, String] =
    Transform.sequence(ot.map(t => visitType(t))).map(_.getOrElse(ShowOps.CNull))

object ASTSerializeVisitor:

  def make(meta: Meta): ASTSerializeVisitor =
    new ASTSerializeVisitor(meta)

  final case class ASTSerializeState(data: String)

  object ASTSerializeState:
    val empty: ASTSerializeState = ASTSerializeState("")

    def make(): ASTSerializeState =
      ASTSerializeState.empty
