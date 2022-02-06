package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
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
    for {
      expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    } yield UnaryMinus(expr)

  private def visitAdd(s: JObject): Either[Throwable, Add] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Add(lhs, rhs)

  private def visitSub(s: JObject): Either[Throwable, Sub] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Sub(lhs, rhs)

  private def visitMul(s: JObject): Either[Throwable, Mul] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Mul(lhs, rhs)

  private def visitDiv(s: JObject): Either[Throwable, Div] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Div(lhs, rhs)

  private def visitMod(s: JObject): Either[Throwable, Mod] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Mod(lhs, rhs)

  private def visitLess(s: JObject): Either[Throwable, Less] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Less(lhs, rhs)

  private def visitLessEqual(s: JObject): Either[Throwable, LessEqual] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield LessEqual(lhs, rhs)

  private def visitGreater(s: JObject): Either[Throwable, Greater] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Greater(lhs, rhs)

  private def visitGreaterEqual(s: JObject): Either[Throwable, GreaterEqual] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield GreaterEqual(lhs, rhs)

  private def visitEqual(s: JObject): Either[Throwable, Equal] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Equal(lhs, rhs)

  private def visitNotEqual(s: JObject): Either[Throwable, NotEqual] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield NotEqual(lhs, rhs)

  private def visitNot(s: JObject): Either[Throwable, Not] =
    for {
      expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    } yield Not(expr)

  private def visitAnd(s: JObject): Either[Throwable, And] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield And(lhs, rhs)

  private def visitOr(s: JObject): Either[Throwable, Or] =
    for {
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    } yield Or(lhs, rhs)

  private def visitAssign(s: JObject): Either[Throwable, Assign] =
    for {
      id <- visitAST(s \ Keys.id).flatMap(_.asLValue)
      expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    } yield Assign(id, expr)

  private def visitNothingVal(s: JObject): Either[Throwable, NothingVal] =
    Right(NothingVal())

  private def visitVoidVal(s: JObject): Either[Throwable, VoidVal] =
    Right(VoidVal())

  private def visitBoolVal(s: JObject): Either[Throwable, BoolVal] =
    for {
      jStr <- (s \ Keys.value).asJString
      value <- parseBool(jStr.s)
    } yield BoolVal(value)

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
        case `div` =>
          visitDiv(o)
        case `mod` =>
          visitMod(o)
        case `less` =>
          visitLess(o)
        case `lessEq` =>
          visitLessEqual(o)
        case `greater` =>
          visitGreater(o)
        case `greaterEq` =>
          visitGreaterEqual(o)
        case `equal` =>
          visitEqual(o)
        case `notEqual` =>
          visitNotEqual(o)
        case `not` =>
          visitNot(o)
        case `and` =>
          visitAnd(o)
        case `or` =>
          visitOr(o)
        case `assign` =>
          visitAssign(o)
        case `nothingVal` =>
          visitNothingVal(o)
        case `voidVal` =>
          visitVoidVal(o)
        case `boolVal` =>
          visitBoolVal(o)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to AST"))
      }
    } yield ast
  }

  private def visitSymbol(s: JValue): Either[Throwable, Symbol] =
    s match {
      case JString(s) =>
        Right(SymbolRef(s))
      case _ =>
        Left(new SerdeException("Cannot convert JValue to a Symbol"))
    }  

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

}

object ASTDeserializeVisitor {

  val init = Init.getClass.getSimpleName
  val unaryMinus = UnaryMinus.getClass.getSimpleName
  val add = Add.getClass.getSimpleName
  val sub = Sub.getClass.getSimpleName
  val mul = Mul.getClass.getSimpleName
  val div = Div.getClass.getSimpleName
  val mod = Mod.getClass.getSimpleName
  val less = Less.getClass.getSimpleName
  val lessEq = LessEqual.getClass.getSimpleName
  val greater = Greater.getClass.getSimpleName
  val greaterEq = GreaterEqual.getClass.getSimpleName
  val equal = Equal.getClass.getSimpleName
  val notEqual = NotEqual.getClass.getSimpleName
  val not = Not.getClass.getSimpleName
  val and = And.getClass.getSimpleName
  val or = Or.getClass.getSimpleName
  val assign = Assign.getClass.getSimpleName
  val nothingVal = NothingVal.getClass.getSimpleName
  val voidVal = VoidVal.getClass.getSimpleName
  val boolVal = BoolVal.getClass.getSimpleName
  val intVal = IntVal.getClass.getSimpleName
  
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

  def parseBool(s: String): Either[Throwable, Boolean] =
    s match {
      case "true" =>
        Right(true)
      case "false" =>
        Right(false)
      case _ =>
        Left(new SerdeException("Cannot parse Boolean"))
    }
}
