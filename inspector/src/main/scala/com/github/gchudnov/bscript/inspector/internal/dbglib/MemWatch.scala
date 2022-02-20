package com.github.gchudnov.bscript.inspector.internal.dbglib

import com.github.gchudnov.bscript.inspector.InspectorException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.Block
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.interpreter.memory.VoidCell
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.rewriter.Rewriter
import com.github.gchudnov.bscript.rewriter.Predicates
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.interpreter.internal.StashEntry
import com.github.gchudnov.bscript.interpreter.internal.Stash
import com.github.gchudnov.bscript.lang.util.LineOps.split
import com.github.gchudnov.bscript.interpreter.memory.Diff

import scala.util.control.Exception.allCatch
import scala.collection.mutable.Stack

/**
 * Trace memory between function calls.
 */
private[inspector] object MemWatch:
  import Casting.*
  import Block.*
  import AST.*

  private val memWatchMethodName: String = "memWatch"

  /**
   * Rewrites AST with memory tracing capabilities
   */
  def make(path: CellPath, ast0: AST, typeNames: TypeNames): Either[Throwable, AST] =
    val wrapCall     = makeWrapCall(path, typeNames)
    val mapper       = makeMapAST(ast0, wrapCall)
    val memWatchDecl = decl(typeNames)

    allCatch.apply(Rewriter.map(ast0, mapper)).map(ast1 => prependDecl(ast1, memWatchDecl))

  /**
   * MethodDecl for memWatch
   */
  private def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.voidType),
      memWatchMethodName,
      List(
        ArgDecl(TypeRef(typeNames.strType), "methodName"), // name of the method to watch
        ArgDecl(TypeRef(typeNames.i32Type), "action"),     // 1 - enter, 2 - exit
        ArgDecl(TypeRef(typeNames.strType), "path")        // cell-path to watch
      ),
      Block(
        CompiledExpr(callback = MemWatch.memWatch, retType = TypeRef(typeNames.voidType))
      ),
      Seq(ComAnn("Used to watch memory updates"), StdAnn())
    )

  /**
   * {{{
   *   // translates from:
   *   f(...);
   *
   *   // into:
   *   {
   *     memWatch("f", 1);  // stores memory state before f()
   *     auto r = f(...);
   *     memWatch("f", 2);  // stores memory state after f() and calculates diff
   *     return r;
   *   }
   * }}}
   */
  private def memWatch(s: Any): Either[Throwable, Any] =
    val argMethodName = "methodName" // str
    val argAction     = "action"     // i32
    val argPath       = "path"       // str

    s match
      case s @ InterpretState(_, stash, ms, c) =>
        for
          methodNameCell <- ms.tryFetch(CellPath(argMethodName))
          actionCell     <- ms.tryFetch(CellPath(argAction))
          pathCell       <- ms.tryFetch(CellPath(argPath))
          newEntry <- (methodNameCell, actionCell, pathCell) match
                        case (StrCell(methodName), IntCell(action), StrCell(path)) =>
                          val entry       = MemWatchStashEntry.get(stash)
                          val watchedCell = ms.fetch(CellPath(path))
                          val stack       = entry.calls.getOrElse(methodName, Stack.empty[Option[Cell]])
                          val newEntry =
                            (
                              if (action == 1) then
                                val newStack = stack.push(watchedCell)
                                entry.copy(calls = entry.calls + (methodName -> newStack))
                              else
                                val prevWatchedCell = stack.pop()
                                val diffs           = Cell.diff(path, prevWatchedCell, watchedCell).toList
                                entry.copy(calls = entry.calls + (methodName -> stack), log = entry.log :+ MemWatchDiff(methodName, CellPath(path), diffs))
                            )
                          Right(newEntry)
                        case other =>
                          Left(new InspectorException(s"Unexpected type of arguments passed to memWatch: ${other}"))

          newStash = Stash(stash.value + (MemWatchStashEntry.name -> newEntry))
          retVal   = VoidCell
        yield s.copy(memSpace = ms, stash = newStash, retValue = retVal)

      case s: Scala2State =>
        for lines <- Right(
                       split(
                         s"""// no-op; the implementation is B1 specific
                            |//        and cannot be expressed in scala
                            |""".stripMargin
                       )
                     )
        yield s.copy(lines = lines)

      case other =>
        Left(new InspectorException(s"Unexpected state passed to memWatch: ${other}"))

  /**
   * 1) If AST is a Block, we prepend MethodDecl to it;
   *
   * 2) If AST is not a Block, we wrap it in a Block and prepend MethodDecl.
   */
  private def prependDecl(ast: AST, decl: MethodDecl): AST =
    ast match
      case b: Block =>
        decl +: b
      case e: Expr =>
        Block(decl, e)
      case other =>
        sys.error("Expected Expr, got AST: " + other) // NOTE: we should not hit this line, since everything is an Expr

  private def makeWrapCall(path: CellPath, typeNames: TypeNames): (Call) => AST = (c: Call) =>
    Block(
      Call(SymbolRef(memWatchMethodName), List(StrVal(c.id.name), IntVal(1), StrVal(path.value))),
      VarDecl(TypeRef(typeNames.autoType), "r", c),
      Call(SymbolRef(memWatchMethodName), List(StrVal(c.id.name), IntVal(2), StrVal(path.value))),
      Var(SymbolRef("r"))
    )

  private def makeMapAST(ast0: AST, wrapCall: Call => AST): (AST) => AST = (n: AST) =>
    n match
      case c: Call =>
        val methodName = c.id.name
        val finder     = makeFindMethodDecl(methodName)

        Rewriter.find(ast0, finder).flatMap(toMethodDecl) match
          case Right(optMd) =>
            optMd match
              case Some(md) =>
                if (!Predicates.hasStdAnn(md)) then wrapCall(c) else c
              case None =>
                throw new InspectorException(s"Could not find MethodDecl '$methodName' for the Call")
          case Left(e) =>
            throw new InspectorException(s"Failed to find MethodDecl '$methodName'", e)

      case other =>
        other

  private def makeFindMethodDecl(name: String): (AST) => Boolean = (n: AST) =>
    n match
      case m: MethodDecl =>
        if (m.name == name) then true
        else false
      case _ =>
        false

  private def toMethodDecl(n: Option[AST]): Either[Throwable, Option[MethodDecl]] =
    Transform.sequence(n.map(_.asMethodDecl))
