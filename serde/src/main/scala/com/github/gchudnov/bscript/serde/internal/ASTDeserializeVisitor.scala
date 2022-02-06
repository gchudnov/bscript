package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{DeclType, Symbol, Type, TypeRef, VectorType}
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.serde.SerdeException
import com.github.gchudnov.bscript.serde.internal.Keys.*
import com.github.gchudnov.bscript.serde.internal.ASTDeserializeVisitor.*
import com.github.gchudnov.bscript.lang.types.VisitorOps.*
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

import scala.util.control.Exception.allCatch


final class ASTDeserializeVisitor {

  private def visitInit(s: JObject): Either[Throwable, Init] =
    for
      iType         <- visitType(s \ Keys.xType)
    yield Init(iType)

  private def visitUnaryMinus(s: JObject): Either[Throwable, UnaryMinus] =
    ???
//    visitUnOp(n.getClass.getSimpleName, n)

  private def visitAdd(s: JObject): Either[Throwable, Add] =
    ???
//    visitBinOp(n.getClass.getSimpleName, n)

  private def visitSub(s: JObject): Either[Throwable, Sub] =
    ???
//    visitBinOp(n.getClass.getSimpleName, n)

  private def visitMul(s: JObject): Either[Throwable, Mul] =
    ???
//    visitBinOp(n.getClass.getSimpleName, n)

  private def visitDiv(s: JObject): Either[Throwable, Div] =
    ???
//    visitBinOp(n.getClass.getSimpleName, n)

  private def visitMod(s: JObject): Either[Throwable, Mod] =
    ???
//    visitBinOp(n.getClass.getSimpleName, n)

  private def visitLess(s: JObject): Either[Throwable, Less] =
    ???
//    visitRelOp(n.getClass.getSimpleName, n)

  private def visitLessEqual(s: JObject): Either[Throwable, LessEqual] =
    ???
//    visitRelOp(n.getClass.getSimpleName, n)

  private def visitGreater(s: JObject): Either[Throwable, Greater] =
    ???
//    visitRelOp(n.getClass.getSimpleName, n)

  private def visitGreaterEqual(s: JObject): Either[Throwable, GreaterEqual] =
    ???
//    visitRelOp(n.getClass.getSimpleName, n)

  private def visitEqual(s: JObject): Either[Throwable, Equal] =
    ???
//    visitRelOp(n.getClass.getSimpleName, n)

  private def visitNotEqual(s: JObject): Either[Throwable, NotEqual] =
    ???
    // visitRelOp(n.getClass.getSimpleName, n)

  private def visitNot(s: JObject): Either[Throwable, Not] =
    ???
//    visitUnLogicOp(n.getClass.getSimpleName, n)

  private def visitAnd(s: JObject): Either[Throwable, And] =
    ???
//    visitLogicOp(n.getClass.getSimpleName, n)

  private def visitOr(s: JObject): Either[Throwable, Or] =
    ???
//    visitLogicOp(n.getClass.getSimpleName, n)

  private def visitAssign(s: JObject): Either[Throwable, Assign] =
    ???
//    for
//      id            <- n.id.visit(ASTSerializeState.empty, this).map(_.data)
//      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitNothingVal(s: JObject): Either[Throwable, NothingVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitVoidVal(s: JObject): Either[Throwable, VoidVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitBoolVal(s: JObject): Either[Throwable, BoolVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitIntVal(s: JObject): Either[Throwable, IntVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitLongVal(s: JObject): Either[Throwable, LongVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitFloatVal(s: JObject): Either[Throwable, FloatVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitDoubleVal(s: JObject): Either[Throwable, DoubleVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitDecimalVal(s: JObject): Either[Throwable, DecimalVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitStrVal(s: JObject): Either[Throwable, StrVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitDateVal(s: JObject): Either[Throwable, DateVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitDateTimeVal(s: JObject): Either[Throwable, DateTimeVal] =
    ???
//    visitConstVal(n.getClass.getSimpleName, n)

  private def visitVec(s: JObject): Either[Throwable, Vec] =
    ???
//    for
//      elements      <- Transform.sequence(n.elements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
//      elementType   <- visitType(n.elementType)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.elements -> elements) ~ (Keys.elementType -> elementType) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitVar(s: JObject): Either[Throwable, Var] =
    ???
//    for
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitArgDecl(s: JObject): Either[Throwable, ArgDecl] =
    ???
//    for
//      aType         <- visitType(n.aType)
//      name          <- Right(n.name)
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> aType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitVarDecl(s: JObject): Either[Throwable, VarDecl] =
    ???
//    for
//      vType         <- visitType(n.vType)
//      name          <- Right(n.name)
//      expr          <- n.expr.visit(ASTSerializeState.empty, this).map(_.data)
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> vType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.expr -> expr) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitFieldDecl(s: JObject): Either[Throwable, FieldDecl] =
    ???
//    for
//      fType         <- visitType(n.fType)
//      name          <- Right(n.name)
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.xType -> fType) ~ (Keys.name -> name) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitMethodDecl(s: JObject): Either[Throwable, MethodDecl] =
    ???
//    for
//      retType       <- visitType(n.retType)
//      name          <- Right(n.name)
//      params        <- Transform.sequence(n.params.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
//      body          <- n.body.visit(ASTSerializeState.empty, this).map(_.data)
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType) ~ (Keys.name -> name) ~ (Keys.params -> params) ~ (Keys.symbol -> symbol) ~ (Keys.body -> body) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitStructDecl(s: JObject): Either[Throwable, StructDecl] =
    ???
//    for
//      name          <- Right(n.name)
//      fields        <- Transform.sequence(n.fields.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.name -> name) ~ (Keys.fields -> fields) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitBlock(s: JObject): Either[Throwable, Block] =
    ???
//    for
//      statements    <- Transform.sequence(n.statements.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
//      symbol        <- visitSymbol(n.symbol)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.name -> name) ~ (Keys.statements -> statements) ~ (Keys.symbol -> symbol) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitCall(s: JObject): Either[Throwable, Call] =
    ???
//    for
//      id            <- visitSymbol(n.id)
//      args          <- Transform.sequence(n.args.map(n1 => n1.visit(ASTSerializeState.empty, this).map(_.data)))
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.id -> id) ~ (Keys.args -> args) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitIf(s: JObject): Either[Throwable, If] =
    ???
//    for
//      cond          <- n.cond.visit(ASTSerializeState.empty, this).map(_.data)
//      then1         <- n.then1.visit(ASTSerializeState.empty, this).map(_.data)
//      else1         <- Transform.sequence(n.else1.map(_.visit(ASTSerializeState.empty, this).map(_.data))).map(_.getOrElse(JNull))
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1 =
//        ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.cond -> cond) ~ (Keys.then1 -> then1) ~ (Keys.else1 -> else1) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitAccess(s: JObject): Either[Throwable, Access] =
    ???

  //    for
//      a             <- n.a.visit(ASTSerializeState.empty, this).map(_.data)
//      b             <- n.b.visit(ASTSerializeState.empty, this).map(_.data)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.a -> a) ~ (Keys.b -> b) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitCompiledExpr(s: JObject): Either[Throwable, CompiledExpr] =
    ???
//    for
//      retType       <- visitType(n.retType)
//      evalType      <- visitType(n.evalType)
//      promoteToType <- visitOptType(n.promoteToType)
//      s1             = ((Keys.kind -> n.getClass.getSimpleName) ~ (Keys.retType -> retType) ~ (Keys.evalType -> evalType) ~ (Keys.promoteToType -> promoteToType))
//    yield ASTSerializeState(data = s1)

  private def visitAST(value: JValue): Either[Throwable, AST] = {
    for {
      o <- value.asJObject
      kind <- (o \ Keys.kind).asJString.map(_.s)
      ast <- kind match {
        case `init` =>
          visitInit(o)
        case `unaryMinus` =>
          visitUnaryMinus(o)
        case `add` =>
          visitAdd(o)
        case `sub` =>
          visitSub(o)
        case `mul` =>
          visitMul(o)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to AST"))
      }
    } yield ast
  }

  private def visitSymbol(value: JValue): Either[Throwable, Symbol] =
    ???  

  private def visitType(s: JValue): Either[Throwable, Type] = {
    s match {
      case o: JObject =>
        for {
          kind <- (o \ Keys.kind).asJString.map(_.s)
          t <- kind match {
              case `vectorType` =>
                val jElementType = o \ Keys.elementType
                visitType(jElementType).map(t => VectorType(t))
              case `declType` =>
                val jExpr = o \ Keys.expr
                visitAST(jExpr).flatMap(_.asExpr).map(expr => DeclType(expr))
          }
        } yield t
      case JString(value) =>
        Right(TypeRef(value))
      case _ =>
        Left(new SerdeException("Cannot extract Type from JValue"))
    }
  }

//  private def visitType(t: Type): Either[Throwable, JValue] =
//    t match
//      case x @ VectorType(elementType) =>
//        for
//          resolvedElementType <- visitType(elementType)
//          s1                   = ((Keys.kind -> x.getClass.getSimpleName) ~ (Keys.elementType -> resolvedElementType))
//        yield s1
//      case x @ DeclType(expr) =>
//        for
//          resolvedExpr <- expr.visit(ASTSerializeState.empty, this).map(_.data)
//          s1            = ((Keys.kind -> x.getClass.getSimpleName) ~ (Keys.expr -> resolvedExpr))
//        yield s1
//      case _ =>
//        Right(JString(t.name))

}

object ASTDeserializeVisitor {

  val init = Init.getClass.getSimpleName
  val unaryMinus = UnaryMinus.getClass.getSimpleName
  val add = Add.getClass.getSimpleName
  val sub = Sub.getClass.getSimpleName
  val mul = Mul.getClass.getSimpleName
  val vectorType = VectorType.getClass.getSimpleName
  val declType = DeclType.getClass.getSimpleName

  implicit class JValueOps(value: JValue) {

    def asJString: Either[Throwable, JString] = {
      value match {
        case s: JString =>
          Right(s)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to JString"))
      }
    }

    def asJObject: Either[Throwable, JObject] = {
      value match {
        case o: JObject =>
          Right(o)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to JObject"))
      }
    }
  }

}
