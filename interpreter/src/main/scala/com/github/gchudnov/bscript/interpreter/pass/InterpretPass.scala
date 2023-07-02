package com.github.gchudnov.bscript.interpreter.pass

import com.github.gchudnov.bscript.interpreter.util.ConstConv
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.lang.func.AstFolder
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
private final class InterpretFolder() extends AstFolder[InterpretState]:

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
        foldAST(foldAST(s, lhs), rhs).storeRetVal(Path(lhs.path)).withRetVal(Cell.Void) // TODO: DONE, remove the comment

      case x: Block =>
        foldOverAST(s, x)

      case x @ Call(id, args) =>
        foldOverAST(s, x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)
      case x: If =>
        foldOverAST(s, x)
      case x @ Init() =>
        foldOverAST(s, x)
      case x @ KeyValue(key, value) =>
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
        throw new MatchError(s"Unsupported AST type in InterpretFolder: ${other}")

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
   * @param name
   *   the name of the value
   * @return
   *   a new state
   */
  def loadRetVal(name: String): InterpretState =
    val rv = area.tryGet(name).toTry.get
    copy(retValue = rv)

  /**
   * Store the return-value in the memory area
   *
   * @param name
   *   the name of the value
   * @return
   *   a new state
   */
  def storeRetVal(name: String): InterpretState =
    copy(area = area.put(name, retValue))

  /**
   * Store the return-value in the memory area
   * @param path
   *   the path of the value
   * @return
   *   a new state
   */
  def storeRetVal(path: Path): InterpretState =
    copy(area = area.tryUpdate(path, retValue).toTry.get)

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
