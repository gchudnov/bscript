package com.github.gchudnov.bscript.interpreter.pass

import com.github.gchudnov.bscript.interpreter.util.ConstConv
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.lang.func.ASTFolder
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.interpreter.InterpreterException
import com.github.gchudnov.bscript.lang.ast.refs.*
import com.github.gchudnov.bscript.interpreter.env.*

/**
 * Interpret Pass
 */
final class InterpretPass extends Pass[HasAST, HasRetValue]:

  override def run(in: HasAST): HasRetValue =
    val initRet  = Cell.Void
    val initArea = Area("#")

    val state0 = InterpretState.from(retValue = initRet, area = initArea)
    val ast0   = in.ast

    val folder = new InterpretFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = new HasRetValue:
      override def retValue: Cell =
        state1.retValue

    out

/**
 * Interpret Resolve Folder
 */
private final class InterpretFolder() extends ASTFolder[InterpretState]:

  override def foldAST(s: InterpretState, ast: AST): InterpretState =
    ast match
      case x: Access =>
        foldOverAST(s, x)

      case Id(name) =>
        s.loadRetVal(name) // TODO: DONE, remove the comment

      case x @ BuiltInDecl(name, bType) =>
        foldOverAST(s, x)
      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s, x)
      case x @ StructDecl(name, sType) =>
        foldOverAST(s, x)

      case VarDecl(name, vType, expr) =>
        foldAST(foldAST(s, vType), expr).storeRetVal(name).withRetVal(Cell.Void) // TODO: DONE, remove the comment

      case x @ TypeDecl(name, tType) =>
        foldOverAST(s, x)

      case x: Annotated =>
        foldOverAST(s, x)

      case Assign(lhs, rhs) =>
        foldAST(foldAST(s, lhs), rhs).updateRetVal(Path(lhs.path)).withRetVal(Cell.Void) // TODO: DONE, remove the comment

      case x: Block =>
        foldOverAST(s, x)

      case x @ Call(id, args) =>
        foldOverAST(s, x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)

      case x: If =>
        interpretIf(s, x) // TODO: DONE, remove the comment

      case x @ Init() =>
        foldOverAST(s, x)
      case x @ Pair(key, value) =>
        foldOverAST(s, x)

      case ConstLit(const) =>
        s.withRetVal(ConstConv.toCell(const)) // TODO: DONE, remove the comment

      case x @ CollectionLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s, x)

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ BuiltInType(name) =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        foldOverAST(s, x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)
      case x @ GenericType(name) =>
        foldOverAST(s, x)

      case other =>
        foldOverAST(s, other)

      // case other =>
      //   throw new MatchError(s"Unsupported AST type in InterpretFolder: ${other}")

  /**
   * Interpret an if statement.
   *
   * @param x
   *   the if statement
   * @return
   *   a new state
   */
  private def interpretIf(s: InterpretState, x: If): InterpretState =
    val s1 = foldAST(s, x.cond)
    s1.retValue match
      case Cell.Bool(cond) =>
        if cond then foldAST(s1, x.then1)
        else foldAST(s1, x.else1)
      case other =>
        throw InterpreterException(s"Condition must be a boolean value, but got: ${other}")

/**
 * Interpret State
 */
private final case class InterpretState(retValue: Cell, area: Area):

  /**
   * Sets the return value.
   *
   * @param cell
   *   the return value
   * @return
   *   a new state
   */
  def withRetVal(cell: Cell): InterpretState =
    copy(retValue = cell)

  /**
   * Load the return-value from the memory area
   *
   * @param fromName
   *   the name of the value to load from
   * @return
   *   a new state
   */
  def loadRetVal(fromName: String): InterpretState =
    val rv = area.getOrError(fromName).toTry.get
    copy(retValue = rv)

  /**
   * Store the return-value in the memory area
   *
   * @param toName
   *   the name of the value to save to
   * @return
   *   a new state
   */
  def storeRetVal(toName: String): InterpretState =
    copy(area = area.put(toName, retValue))

  /**
   * Update the return-value in the memory area
   *
   * The assignment possible if:
   *   - the old value is of the same type as the new value
   *   - the old value is of type Nothing
   *   - the new value is of type Nothing
   *
   * @param toPath
   *   the path of the value to store in
   * @return
   *   a new state
   */
  def updateRetVal(toPath: Path): InterpretState =
    val errOrArea = for
      oldValue <- area.getOrError(toPath)
      _ <- Either.cond(
             Cell.isSameType(oldValue, retValue) || Cell.isSameType(oldValue, Cell.Nothing) || Cell.isSameType(retValue, Cell.Nothing),
             (),
             InterpreterException(s"Cannot update value ${oldValue} with value ${retValue}, types are different"),
           )
      newArea <- area.updateOrError(toPath, retValue)
    yield newArea

    val newArea = errOrArea.toTry.get
    copy(area = newArea)

/**
 * Interpret State Companion
 */
private object InterpretState:

  def from(retValue: Cell, area: Area): InterpretState =
    InterpretState(
      retValue = retValue,
      area = area,
    )

// import com.github.gchudnov.bscript.builder.state.Meta
// import com.github.gchudnov.bscript.interpreter.memory.*

// trait StashEntry

// final case class Stash(value: Map[String, StashEntry])

// object Stash:
//   val empty: Stash =
//     Stash(Map.empty[String, StashEntry])

// final case class InterpretState()

// final case class InterpretState(meta: Meta, stash: Stash, memSpace: MemorySpace, retValue: Cell)

// object InterpretState:
//   def make(meta: Meta, stash: Stash, memSpace: MemorySpace, retValue: Cell): InterpretState =
//     InterpretState(
//       meta = meta,
//       stash = stash,
//       memSpace = memSpace,
//       retValue = retValue
//     )
