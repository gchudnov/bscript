package com.github.gchudnov.bscript.interpreter.internal

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.interpreter.internal.InterpretVisitor.InterpretState
import com.github.gchudnov.bscript.interpreter.InterpretLaws
import com.github.gchudnov.bscript.interpreter.laws.*
import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.util.Transform

/**
 * 4-PASS
 *
 * High-Level Interpreter
 *
 * Because symbol table management happens at run-time in an interpreter, it’s easy to confuse resolving a variable with loading its value. Just keep in mind that resolving a
 * variable means figuring out which program entity it refers to. We can do this without even running the program for statically typed languages. Loading a variable, on the other
 * hand, is purely a run-time operation.
 *
 * We resolve a variable to figure out the space in which its value lives.
 *
 * The parser deals with symbol scopes, and the interpreter deals with memory spaces. Memory spaces don’t do double duty as scopes.
 *
 * During execution, though, we still need scope information to resolve symbols.
 */
final class InterpretVisitor(laws: InterpretLaws) extends TreeVisitor[InterpretState, InterpretState]:
  import InterpretVisitor.*
  import com.github.gchudnov.bscript.lang.types.VisitorOps.*

  private val mathLaws: Arithmetic     = laws.mathLaws
  private val boolLaws: BoolArithmetic = laws.boolLaws
  private val cmpLaws: Comparator      = laws.cmpLaws
  private val initLaws: Initializer    = laws.initLaws
  private val typeCaster: TypeCaster   = laws.typeCaster

  override def visit(s: InterpretState, n: Init): Either[Throwable, InterpretState] =
    for x <- initLaws.init(n.iType)
    yield s.copy(retValue = x)

  override def visit(s: InterpretState, n: UnaryMinus): Either[Throwable, InterpretState] =
    for
      s1 <- n.expr.visit(s, this)
      x   = s1.retValue
      y  <- mathLaws.unaryMinus(x).flatMap(it => promote(it, n.promoteToType))
    yield s1.copy(retValue = y)

  override def visit(s: InterpretState, n: Add): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- mathLaws.add(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Sub): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- mathLaws.sub(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Mul): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- mathLaws.mul(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Div): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- mathLaws.div(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Mod): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- mathLaws.mod(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Less): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.less(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: LessEqual): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.lessEqual(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Greater): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.greater(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: GreaterEqual): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.greaterEqual(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Equal): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.equal(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: NotEqual): Either[Throwable, InterpretState] =
    for
      ls <- n.lhs.visit(s, this)
      rs <- n.rhs.visit(ls, this)
      x   = ls.retValue
      y   = rs.retValue
      z  <- cmpLaws.notEqual(x, y).flatMap(it => promote(it, n.promoteToType))
    yield rs.copy(retValue = z)

  override def visit(s: InterpretState, n: Not): Either[Throwable, InterpretState] =
    for
      s1 <- n.expr.visit(s, this)
      x   = s1.retValue
      z  <- boolLaws.not(x).flatMap(it => promote(it, n.promoteToType))
    yield s1.copy(retValue = z)

  override def visit(s: InterpretState, n: And): Either[Throwable, InterpretState] =
    for
      ls    <- n.lhs.visit(s, this)
      x      = ls.retValue
      xBool <- x.asBoolean
      zs <- (if xBool then
               for
                 rs <- n.rhs.visit(ls, this)
                 y   = rs.retValue
                 z  <- boolLaws.and(x, y).flatMap(it => promote(it, n.promoteToType))
               yield rs.copy(retValue = z)
             else
               for z <- promote(x, n.promoteToType)
               yield ls.copy(retValue = z)
            )
    yield zs

  override def visit(s: InterpretState, n: Or): Either[Throwable, InterpretState] =
    for
      ls    <- n.lhs.visit(s, this)
      x      = ls.retValue
      xBool <- x.asBoolean
      zs <- (if !xBool then
               for
                 rs <- n.rhs.visit(ls, this)
                 y   = rs.retValue
                 z  <- boolLaws.or(x, y).flatMap(it => promote(it, n.promoteToType))
               yield rs.copy(retValue = z)
             else
               for z <- promote(x, n.promoteToType)
               yield ls.copy(retValue = z)
            )
    yield zs

  override def visit(s: InterpretState, n: Assign): Either[Throwable, InterpretState] =
    for
      es <- n.expr.visit(s, this)
      ms <- n.id match
              case v: Var =>
                promote(es.retValue, n.expr.promoteToType)
                  .flatMap(promotedValue => es.memSpace.tryUpdate(v.symbol.name, promotedValue))
              case a: Access =>
                promote(es.retValue, n.expr.promoteToType)
                  .flatMap(promotedValue => es.memSpace.patch(CellPath(a.path), promotedValue))
              case other =>
                Left(new AstException(s"Cannot Assign a value '${n.expr}' to '${other}'"))
    yield es.copy(memSpace = ms, retValue = VoidCell)

  override def visit(s: InterpretState, n: NothingVal): Either[Throwable, InterpretState] =
    for z <- promote(NothingCell, n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: VoidVal): Either[Throwable, InterpretState] =
    for z <- promote(VoidCell, n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: BoolVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: IntVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: LongVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: FloatVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: DoubleVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: DecimalVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: StrVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: DateVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: DateTimeVal): Either[Throwable, InterpretState] =
    for z <- promote(Cell(n.value), n.promoteToType)
    yield s.copy(retValue = z)

  override def visit(s: InterpretState, n: Vec): Either[Throwable, InterpretState] =
    for
      sc <- n.elements.foldLeft(Right(s, Vector.empty[Cell]): Either[Throwable, (InterpretState, Vector[Cell])]) { (acc, expr) =>
              acc match
                case Left(ex)           => Left(ex)
                case Right((sx, cells)) => expr.visit(sx, this).map(sy => (sy, cells :+ sy.retValue))
            }
      (s1, exprCells) = sc
      elemCells      <- Transform.sequence(exprCells.map(it => promote(it, Some(n.elementType))))
      promotedCells  <- Transform.sequence(elemCells.map(it => promote(it, n.promoteToType)))
      z               = Cell(promotedCells)
    yield s1.copy(retValue = z)

  override def visit(s: InterpretState, n: Var): Either[Throwable, InterpretState] =
    for
      z  <- s.memSpace.tryGet(n.symbol.name)
      pz <- promote(z, n.promoteToType)
    yield s.copy(retValue = pz)

  override def visit(s: InterpretState, n: ArgDecl): Either[Throwable, InterpretState] =
    Right(s.copy(retValue = VoidCell))

  override def visit(s: InterpretState, n: VarDecl): Either[Throwable, InterpretState] =
    for
      s1 <- n.expr.visit(s, this)
      ms  = s1.memSpace.put(n.name, s1.retValue)
    yield s1.copy(memSpace = ms, retValue = VoidCell)

  override def visit(s: InterpretState, n: FieldDecl): Either[Throwable, InterpretState] =
    Right(s.copy(retValue = VoidCell))

  override def visit(s: InterpretState, n: MethodDecl): Either[Throwable, InterpretState] =
    Right(s.copy(retValue = VoidCell))

  override def visit(s: InterpretState, n: StructDecl): Either[Throwable, InterpretState] =
    Right(s.copy(retValue = VoidCell))

  override def visit(s: InterpretState, n: Block): Either[Throwable, InterpretState] =
    for
      s1 <- n.statements
              .foldLeft(Right(s.copy(memSpace = MemorySpace(n.symbol.name, Some(s.memSpace)), retValue = VoidCell)): Either[Throwable, InterpretState]) { (acc, stmt) =>
                acc match
                  case Left(err) =>
                    Left(err)
                  case Right(sx) =>
                    stmt.visit(sx, this)
              }
      ms <- s1.memSpace.pop()
    yield s1.copy(memSpace = ms)

  override def visit(s: InterpretState, n: Call): Either[Throwable, InterpretState] =
    for
      sMethod    <- n.id.asSMethod
      methodDecl <- s.meta.methodAst(sMethod).flatMap(_.asMethodDecl)
      sVars      <- s.meta.methodArgSVars(sMethod)
      s1 <- sVars
              .zip(n.args)
              .foldLeft(Right(s.copy(memSpace = MemorySpace(n.id.name, Some(s.memSpace)), retValue = VoidCell)): Either[Throwable, InterpretState]) { case (acc, (arg, expr)) =>
                acc match
                  case Left(ex) => Left(ex)
                  case Right(sx) =>
                    for
                      sy <- expr.visit(sx, this)
                      ms  = sy.memSpace.put(arg.name, sy.retValue)
                    yield sy.copy(memSpace = ms, retValue = VoidCell)
              }
      s2       <- methodDecl.body.visit(s1, this)
      retValue <- promote(s2.retValue, n.promoteToType)
      ms       <- s1.memSpace.pop()
    yield s2.copy(memSpace = ms, retValue = retValue)

  override def visit(s: InterpretState, n: If): Either[Throwable, InterpretState] =
    for
      cs <- n.cond.visit(s, this)
      x  <- cs.retValue.asBoolean
      rs <- if x then n.then1.visit(cs, this)
            else n.else1.fold(Right(cs): Either[Throwable, InterpretState])(_.visit(cs, this))
    yield rs

  override def visit(s: InterpretState, n: Access): Either[Throwable, InterpretState] =
    for cell <- s.memSpace.fetch(CellPath(n.path))
    yield s.copy(retValue = cell)

  override def visit(s: InterpretState, n: CompiledExpr): Either[Throwable, InterpretState] =
    for rs <- n.callback(s).flatMap(_.asInterpretState)
    yield rs

  private def promote(cell: Cell, promoteToType: Option[Type]): Either[Throwable, Cell] =
    promoteToType.fold[Either[Throwable, Cell]](Right(cell))(t => typeCaster.cast(cell, t))

object InterpretVisitor:

  def make(laws: InterpretLaws): InterpretVisitor =
    new InterpretVisitor(laws)

  final case class InterpretState(meta: Meta, memSpace: MemorySpace, retValue: Cell)

  object InterpretState:
    def make(meta: Meta, memSpace: MemorySpace, retValue: Cell): InterpretState =
      InterpretState(
        meta = meta,
        memSpace = memSpace,
        retValue = retValue
      )

  implicit class AnyOps(a: Any):
    def asInterpretState: Either[Throwable, InterpretState] =
      a match
        case x: InterpretState => Right(x)
        case _                 => Left(new AstException("Cannot cast Any to InterpretState"))
