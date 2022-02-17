package com.github.gchudnov.bscript.inspector.internal.dbglib

import com.github.gchudnov.bscript.inspector.InspectorException
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.Block
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.interpreter.memory.*
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.rewriter.Rewriter
import com.github.gchudnov.bscript.rewriter.Predicates
import com.github.gchudnov.bscript.lang.util.{ Casting, Transform }
import com.github.gchudnov.bscript.interpreter.internal.InterpretState
import com.github.gchudnov.bscript.translator.internal.scala2.Scala2State
import com.github.gchudnov.bscript.lang.util.LineOps.split

import scala.util.control.Exception.allCatch
import com.github.gchudnov.bscript.interpreter.memory.VoidCell

/**
 * Trace memory between function calls.
 */
private[inspector] object MemWatch:
  import Casting.*
  import Block.*

  /**
   * Rewrites AST with memory tracing capabilities
   */
  def make(ast0: AST, typeNames: TypeNames): Either[Throwable, AST] =
    val wrapCall     = makeWrapCall(typeNames)
    val mapper       = makeMapAST(ast0, wrapCall)
    val memWatchDecl = decl(typeNames)

    allCatch.apply(Rewriter.map(ast0, mapper)).map(ast1 => prependDecl(ast1, memWatchDecl))

  /**
   * MethodDecl for memWatch
   */
  private def decl(typeNames: TypeNames): MethodDecl =
    MethodDecl(
      TypeRef(typeNames.voidType),
      "memWatch",
      List(
        ArgDecl(TypeRef(typeNames.strType), "methodName"), // name of the method to watch
        ArgDecl(TypeRef(typeNames.i32Type), "action")      // 1 - enter, 2 - exit
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
   *     memWatch("f", 1);
   *     auto r = f(...);
   *     memWatch("f", 2);
   *     return r;
   *   }
   * }}}
   */
  private def memWatch(s: Any): Either[Throwable, Any] =
    val argMethodName = "methodName" // str
    val argAction     = "action"     // i32

    s match
      case s @ InterpretState(_, ms, c) =>
        for
          methodNameCell <- ms.fetch(CellPath(argMethodName))
          actionCell     <- ms.fetch(CellPath(argAction))
          retVal          = VoidCell
        yield s.copy(memSpace = ms, retValue = retVal)

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

  private def makeWrapCall(typeNames: TypeNames): (Call) => AST = (c: Call) =>
    Block(
      Call(SymbolRef("memWatch"), List(StrVal(c.id.name), IntVal(1))),
      VarDecl(TypeRef(typeNames.autoType), "r", c),
      Call(SymbolRef("memWatch"), List(StrVal(c.id.name), IntVal(2))),
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
                if (Predicates.hasStdAnn(md)) then wrapCall(c) else c
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
