package com.github.gchudnov.bscript.serde.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.lang.symbols.{ DeclType, Symbol, Type, TypeRef, VectorType }
import com.github.gchudnov.bscript.lang.util.Transform
import com.github.gchudnov.bscript.serde.SerdeException
import com.github.gchudnov.bscript.serde.internal.Keys.*
import com.github.gchudnov.bscript.serde.internal.JSONDeserializeVisitor.*
import com.github.gchudnov.bscript.lang.util.Casting
import org.json4s.*
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods.*

import scala.util.control.Exception.allCatch
import java.time.LocalDate
import java.time.OffsetDateTime
import scala.collection.immutable.MapOps

private[internal] final class JSONDeserializeVisitor:
  import Casting.*

  private def visitInit(s: JObject): Either[Throwable, Init] =
    for iType <- visitType(s \ Keys.xType)
    yield Init(iType)

  private def visitUnaryMinus(s: JObject): Either[Throwable, UnaryMinus] =
    for expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    yield UnaryMinus(expr)

  private def visitAdd(s: JObject): Either[Throwable, Add] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Add(lhs, rhs)

  private def visitSub(s: JObject): Either[Throwable, Sub] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Sub(lhs, rhs)

  private def visitMul(s: JObject): Either[Throwable, Mul] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Mul(lhs, rhs)

  private def visitDiv(s: JObject): Either[Throwable, Div] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Div(lhs, rhs)

  private def visitMod(s: JObject): Either[Throwable, Mod] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Mod(lhs, rhs)

  private def visitLess(s: JObject): Either[Throwable, Less] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Less(lhs, rhs)

  private def visitLessEqual(s: JObject): Either[Throwable, LessEqual] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield LessEqual(lhs, rhs)

  private def visitGreater(s: JObject): Either[Throwable, Greater] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Greater(lhs, rhs)

  private def visitGreaterEqual(s: JObject): Either[Throwable, GreaterEqual] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield GreaterEqual(lhs, rhs)

  private def visitEqual(s: JObject): Either[Throwable, Equal] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Equal(lhs, rhs)

  private def visitNotEqual(s: JObject): Either[Throwable, NotEqual] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield NotEqual(lhs, rhs)

  private def visitNot(s: JObject): Either[Throwable, Not] =
    for expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    yield Not(expr)

  private def visitAnd(s: JObject): Either[Throwable, And] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield And(lhs, rhs)

  private def visitOr(s: JObject): Either[Throwable, Or] =
    for
      lhs <- visitAST(s \ Keys.lhs).flatMap(_.asExpr)
      rhs <- visitAST(s \ Keys.rhs).flatMap(_.asExpr)
    yield Or(lhs, rhs)

  private def visitAssign(s: JObject): Either[Throwable, Assign] =
    for
      id   <- visitAST(s \ Keys.id).flatMap(_.asLValue)
      expr <- visitAST(s \ Keys.expr).flatMap(_.asExpr)
    yield Assign(id, expr)

  private def visitNothingVal(s: JObject): Either[Throwable, NothingVal] =
    Right(NothingVal())

  private def visitVoidVal(s: JObject): Either[Throwable, VoidVal] =
    Right(VoidVal())

  private def visitBoolVal(s: JObject): Either[Throwable, BoolVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseBool(jStr.s)
    yield BoolVal(value)

  private def visitIntVal(s: JObject): Either[Throwable, IntVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseInt(jStr.s)
    yield IntVal(value)

  private def visitLongVal(s: JObject): Either[Throwable, LongVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseLong(jStr.s)
    yield LongVal(value)

  private def visitFloatVal(s: JObject): Either[Throwable, FloatVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseFloat(jStr.s)
    yield FloatVal(value)

  private def visitDoubleVal(s: JObject): Either[Throwable, DoubleVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseDouble(jStr.s)
    yield DoubleVal(value)

  private def visitDecimalVal(s: JObject): Either[Throwable, DecimalVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseDecimal(jStr.s)
    yield DecimalVal(value)

  private def visitStrVal(s: JObject): Either[Throwable, StrVal] =
    for
      jStr <- (s \ Keys.value).asJString
      value = jStr.s
    yield StrVal(value)

  private def visitDateVal(s: JObject): Either[Throwable, DateVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseDate(jStr.s)
    yield DateVal(value)

  private def visitDateTimeVal(s: JObject): Either[Throwable, DateTimeVal] =
    for
      jStr  <- (s \ Keys.value).asJString
      value <- parseDateTime(jStr.s)
    yield DateTimeVal(value)

  private def visitStructVal(s: JObject): Either[Throwable, StructVal] =
    for
      jType <- Right(s \ Keys.xType)
      sType <- visitType(jType)
      jObj  <- (s \ Keys.value).asJObject
      value <- Transform.sequence(jObj.obj.map { case (name, jEl) => visitAST(jEl).flatMap(_.asExpr).map(expr => (name, expr)) }).map(_.toMap)
    yield StructVal(sType = sType, value = value)

  private def visitVec(s: JObject): Either[Throwable, Vec] =
    for
      jElements <- (s \ Keys.elements).asJArray
      elements  <- Transform.sequence(jElements.arr.map(jEl => visitAST(jEl).flatMap(_.asExpr)))
      optElementType <- (s \ Keys.elementType) match
                          case JNothing =>
                            Right(None)
                          case jType =>
                            visitType(jType).map(Some(_))
      ast = optElementType match
              case None =>
                Vec(elements)
              case Some(elementType) =>
                Vec(elements, elementType)
    yield ast

  private def visitVar(s: JObject): Either[Throwable, Var] =
    for
      jSymbol <- Right(s \ Keys.symbol)
      symbol  <- visitSymbol(jSymbol)
    yield Var(symbol)

  private def visitArgDecl(s: JObject): Either[Throwable, ArgDecl] =
    for
      jType <- Right(s \ Keys.xType)
      aType <- visitType(jType)
      name  <- (s \ Keys.name).asJString.map(_.s)
    yield ArgDecl(aType, name)

  private def visitVarDecl(s: JObject): Either[Throwable, VarDecl] =
    for
      jType <- Right(s \ Keys.xType)
      vType <- visitType(jType)
      name  <- (s \ Keys.name).asJString.map(_.s)
      jExpr <- Right(s \ Keys.expr)
      expr  <- visitAST(jExpr).flatMap(_.asExpr)
    yield VarDecl(vType, name, expr)

  private def visitFieldDecl(s: JObject): Either[Throwable, FieldDecl] =
    for
      jType <- Right(s \ Keys.xType)
      fType <- visitType(jType)
      name  <- (s \ Keys.name).asJString.map(_.s)
    yield FieldDecl(fType, name)

  private def visitMethodDecl(s: JObject): Either[Throwable, MethodDecl] =
    for
      jRetType <- Right(s \ Keys.retType)
      retType  <- visitType(jRetType)
      name     <- (s \ Keys.name).asJString.map(_.s)
      jParams  <- (s \ Keys.params).asJArray
      params   <- Transform.sequence(jParams.arr.map(jEl => visitAST(jEl).flatMap(_.asArgDecl)))
      jBody    <- (s \ Keys.body).asJObject
      body     <- visitAST(jBody).flatMap(_.asBlock)
    yield MethodDecl(retType, name, params, body)

  private def visitStructDecl(s: JObject): Either[Throwable, StructDecl] =
    for
      name    <- (s \ Keys.name).asJString.map(_.s)
      jFields <- (s \ Keys.fields).asJArray
      fields  <- Transform.sequence(jFields.arr.map(jEl => visitAST(jEl).flatMap(_.asFieldDecl)))
    yield StructDecl(name, fields)

  private def visitBlock(s: JObject): Either[Throwable, Block] =
    for
      jStmts <- (s \ Keys.statements).asJArray
      stmts  <- Transform.sequence(jStmts.arr.map(jEl => visitAST(jEl).flatMap(_.asExpr)))
    yield Block(stmts*)

  private def visitCall(s: JObject): Either[Throwable, Call] =
    for
      jId   <- Right(s \ Keys.id)
      id    <- visitSymbol(jId)
      jArgs <- (s \ Keys.args).asJArray
      args  <- Transform.sequence(jArgs.arr.map(jEl => visitAST(jEl).flatMap(_.asExpr)))
    yield Call(id, args)

  private def visitIf(s: JObject): Either[Throwable, If] =
    for
      jCond <- Right(s \ Keys.cond)
      cond  <- visitAST(jCond).flatMap(_.asExpr)
      jThen <- Right(s \ Keys.then1)
      then1 <- visitAST(jThen).flatMap(_.asExpr)
      jElse <- Right(s \ Keys.else1)
      else1 <- jElse match
                 case JNothing =>
                   Right(None)
                 case jElse =>
                   visitAST(jElse).flatMap(_.asExpr).map(Some(_))
    yield If(cond, then1, else1)

  private def visitAccess(s: JObject): Either[Throwable, Access] =
    for
      jA <- Right(s \ Keys.a)
      a  <- visitAST(jA).flatMap(_.asLValue)
      jB <- Right(s \ Keys.b)
      b  <- visitAST(jB).flatMap(_.asLValue)
    yield Access(a, b)

  private def visitCompiledExpr(s: JObject): Either[Throwable, CompiledExpr] =
    for
      jRetType <- Right(s \ Keys.retType)
      retType  <- visitType(jRetType)
      callback  = (a: Any) => Left(new SerdeException("CompiledExpr was not deserialized - it is compiled and not available for serialization"))
    yield CompiledExpr(callback, retType)

  private val nameVisitor: Map[String, (JObject) => Either[Throwable, AST]] =
    Map(
      initName        -> visitInit,
      unaryMinusName  -> visitUnaryMinus,
      addName         -> visitAdd,
      subName         -> visitSub,
      mulName         -> visitMul,
      divName         -> visitDiv,
      modName         -> visitMod,
      lessName        -> visitLess,
      lessEqName      -> visitLessEqual,
      greaterName     -> visitGreater,
      greaterEqName   -> visitGreaterEqual,
      equalName       -> visitEqual,
      notEqualName    -> visitNotEqual,
      notName         -> visitNot,
      andName         -> visitAnd,
      orName          -> visitOr,
      assignName      -> visitAssign,
      nothingValName  -> visitNothingVal,
      voidValName     -> visitVoidVal,
      boolValName     -> visitBoolVal,
      intValName      -> visitIntVal,
      longValName     -> visitLongVal,
      floatValName    -> visitFloatVal,
      doubleValName   -> visitDoubleVal,
      decimalValName  -> visitDecimalVal,
      stringValName   -> visitStrVal,
      dateValName     -> visitDateVal,
      dateTimeValName -> visitDateTimeVal,
      structValName   -> visitStructVal,
      vecName         -> visitVec,
      varName         -> visitVar,
      argDeclName     -> visitArgDecl,
      varDeclName     -> visitVarDecl,
      fieldDeclName   -> visitFieldDecl,
      methodDeclName  -> visitMethodDecl,
      structDeclName  -> visitStructDecl,
      blockName       -> visitBlock,
      callName        -> visitCall,
      ifName          -> visitIf,
      accessName      -> visitAccess,
      comExprName     -> visitCompiledExpr
    )

  def visitAST(value: JValue): Either[Throwable, AST] =
    for
      o    <- value.asJObject
      kind <- (o \ Keys.kind).asJString.map(_.s)
      ast  <- nameVisitor.get(kind).map(_(o)).getOrElse(Left(new Exception(s"Unknown AST kind: '$kind'")))
    yield ast

  private def visitSymbol(s: JValue): Either[Throwable, Symbol] =
    s match
      case JString(s) =>
        Right(SymbolRef(s))
      case _ =>
        Left(new SerdeException("Cannot convert JValue to a Symbol"))

  private def visitType(s: JValue): Either[Throwable, Type] =
    s match
      case o: JObject =>
        for
          kind <- (o \ Keys.kind).asJString.map(_.s)
          t <- kind match
                 case `vectorTypeName` =>
                   val jElementType = o \ Keys.elementType
                   visitType(jElementType).map(t => VectorType(t))
                 case `declTypeName` =>
                   val jExpr = o \ Keys.expr
                   visitAST(jExpr).flatMap(_.asExpr).map(expr => DeclType(expr))
        yield t
      case JString(value) =>
        Right(TypeRef(value))
      case _ =>
        Left(new SerdeException("Cannot extract Type from JValue"))

private[internal] object JSONDeserializeVisitor:

  val initName: String        = classOf[Init].getSimpleName
  val unaryMinusName: String  = classOf[UnaryMinus].getSimpleName
  val addName: String         = classOf[Add].getSimpleName
  val subName: String         = classOf[Sub].getSimpleName
  val mulName: String         = classOf[Mul].getSimpleName
  val divName: String         = classOf[Div].getSimpleName
  val modName: String         = classOf[Mod].getSimpleName
  val lessName: String        = classOf[Less].getSimpleName
  val lessEqName: String      = classOf[LessEqual].getSimpleName
  val greaterName: String     = classOf[Greater].getSimpleName
  val greaterEqName: String   = classOf[GreaterEqual].getSimpleName
  val equalName: String       = classOf[Equal].getSimpleName
  val notEqualName: String    = classOf[NotEqual].getSimpleName
  val notName: String         = classOf[Not].getSimpleName
  val andName: String         = classOf[And].getSimpleName
  val orName: String          = classOf[Or].getSimpleName
  val assignName: String      = classOf[Assign].getSimpleName
  val nothingValName: String  = classOf[NothingVal].getSimpleName
  val voidValName: String     = classOf[VoidVal].getSimpleName
  val boolValName: String     = classOf[BoolVal].getSimpleName
  val intValName: String      = classOf[IntVal].getSimpleName
  val longValName: String     = classOf[LongVal].getSimpleName
  val floatValName: String    = classOf[FloatVal].getSimpleName
  val doubleValName: String   = classOf[DoubleVal].getSimpleName
  val decimalValName: String  = classOf[DecimalVal].getSimpleName
  val stringValName: String   = classOf[StrVal].getSimpleName
  val dateValName: String     = classOf[DateVal].getSimpleName
  val dateTimeValName: String = classOf[DateTimeVal].getSimpleName
  val structValName: String   = classOf[StructVal].getSimpleName
  val vecName: String         = classOf[Vec].getSimpleName
  val varName: String         = classOf[Var].getSimpleName
  val argDeclName: String     = classOf[ArgDecl].getSimpleName
  val varDeclName: String     = classOf[VarDecl].getSimpleName
  val fieldDeclName: String   = classOf[FieldDecl].getSimpleName
  val methodDeclName: String  = classOf[MethodDecl].getSimpleName
  val structDeclName: String  = classOf[StructDecl].getSimpleName
  val vectorTypeName: String  = classOf[VectorType].getSimpleName
  val blockName: String       = classOf[Block].getSimpleName
  val callName: String        = classOf[Call].getSimpleName
  val ifName: String          = classOf[If].getSimpleName
  val accessName: String      = classOf[Access].getSimpleName
  val declTypeName: String    = classOf[DeclType].getSimpleName
  val comExprName: String     = classOf[CompiledExpr].getSimpleName

  def make(): JSONDeserializeVisitor =
    new JSONDeserializeVisitor()

  implicit class JValueOps(value: JValue):

    def asJString: Either[Throwable, JString] =
      value match
        case s: JString =>
          Right(s)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to JString"))

    def asJObject: Either[Throwable, JObject] =
      value match
        case o: JObject =>
          Right(o)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to JObject"))

    def asJArray: Either[Throwable, JArray] =
      value match
        case a: JArray =>
          Right(a)
        case _ =>
          Left(new SerdeException("Cannot convert JValue to JArray"))

  def parseBool(s: String): Either[Throwable, Boolean] =
    s match
      case "true" =>
        Right(true)
      case "false" =>
        Right(false)
      case _ =>
        Left(new SerdeException("Cannot parse Boolean"))

  def parseInt(s: String): Either[Throwable, Int] =
    allCatch
      .either(s.toInt)
      .left
      .map(_ => new SerdeException("Cannot parse Int"))

  def parseLong(s: String): Either[Throwable, Long] =
    allCatch
      .either(s.toLong)
      .left
      .map(_ => new SerdeException("Cannot parse Long"))

  def parseFloat(s: String): Either[Throwable, Float] =
    allCatch
      .either(s.toFloat)
      .left
      .map(_ => new SerdeException("Cannot parse Float"))

  def parseDouble(s: String): Either[Throwable, Double] =
    allCatch
      .either(s.toDouble)
      .left
      .map(_ => new SerdeException("Cannot parse Double"))

  def parseDecimal(s: String): Either[Throwable, BigDecimal] =
    allCatch
      .either(BigDecimal(s))
      .left
      .map(_ => new SerdeException("Cannot parse BigDecimal"))

  def parseDate(s: String): Either[Throwable, LocalDate] =
    allCatch
      .either(LocalDate.parse(s))
      .left
      .map(_ => new SerdeException("Cannot parse LocalDate"))

  def parseDateTime(s: String): Either[Throwable, OffsetDateTime] =
    allCatch
      .either(OffsetDateTime.parse(s))
      .left
      .map(_ => new SerdeException("Cannot parse OffsetDateTime"))
