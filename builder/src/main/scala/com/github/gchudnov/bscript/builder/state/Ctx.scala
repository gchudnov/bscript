package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.ast.AST

final case class Ctx(data: List[String])

/**
 * Context the symbol resides in
 */
object Ctx {}
// import Meta.given

// def make(meta: Meta, scope: Scope): Either[Throwable, Ctx] =
//   val path     = meta.scopeTree.pathToRoot(scope)
//   val sMethods = path.collect { case m: SMethod => m }
//   val data     = List.empty[String] // sMethods.map(sMethod => meta.showMethod(sMethod))
//   Right(Ctx(data))

// def make(meta: Meta, symbol: Symbol): Either[Throwable, Ctx] =
//   for
//     scope <- meta.scopeFor(symbol)
//     ctx   <- make(meta, scope)
//   yield ctx

// def make(meta: Meta, ast: AST): Either[Throwable, Ctx] =
//   for
//     scope <- meta.scopeFor(ast)
//     ctx   <- make(meta, scope)
//   yield ctx

// def str(meta: Meta, scope: Scope): String =
//   toStr(make(meta, scope))

// def str(meta: Meta, ast: AST): String =
//   toStr(make(meta, ast))

// def str(meta: Meta, symbol: Symbol): String =
//   toStr(make(meta, symbol))

// private def toStr(errOrCtx: Either[Throwable, Ctx]): String =
//   errOrCtx
//     .fold(t => s"[context error: ${t.getMessage}]", ctx => if ctx.data.nonEmpty then ctx.data.mkString(" :: ") else "no detail")
