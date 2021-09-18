package com.github.gchudnov.bscript.lang.ast.visitors

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.state.Meta
import com.github.gchudnov.bscript.lang.symbols.{ SMethod, SVar, Scope, Symbol, Type }
import com.github.gchudnov.bscript.lang.util.EqWrap

object SpecUtil:

  def typeNameForVarInScope(meta: Meta)(varName: String, scopeName: String): Either[Throwable, String] =
    for
      scope <- meta.scopeTree.vertices.find(_.value.name == scopeName).map(_.value).toRight(new RuntimeException(s"Scope '${scopeName}' not found."))
      symbol <-
        meta.scopeSymbols.get(EqWrap(scope)).flatMap(_.find(_.name == varName)).toRight(new RuntimeException(s"Symbol '${varName}' not found in Scope '${scopeName}'."))
      sVar  <- Either.cond(symbol.isInstanceOf[SVar], symbol.asInstanceOf[SVar], new RuntimeException(s"Symbol '${varName}' in scope '${scopeName}' is not an SVar."))
      sType <- meta.varTypes.get(EqWrap(sVar)).toRight(new RuntimeException(s"Type is not found for SVar '${varName}'."))
    yield sType.name

  def findSymbolScope(meta: Meta, sym: Symbol): Option[Scope] =
    meta.symbolScopes.find(_._1.value == sym).map(_._2)

  def findSymbolScope(meta: Meta, symName: String): Option[Scope] =
    meta.symbolScopes.find(_._1.value.name == symName).map(_._2)

  def findMember(meta: Meta, name: String, in: Scope): Option[Symbol] =
    meta.resolveMember(name, in).toOption

  def findType(meta: Meta, v: SVar): Option[Type] =
    meta.typeFor(v).toOption

  def findRetType(meta: Meta, m: SMethod): Option[Type] =
    meta.retTypeFor(m).toOption

  def findSymbolScopes(meta: Meta, symName: String): List[Scope] =
    meta.symbolScopes.filter(_._1.value.name == symName).values.toList

  def findSMethodAST(meta: Meta, methodName: String): Option[(SMethod, AST)] =
    meta.methodAsts.find(_._1.value.name == methodName).map(it => (it._1.value, it._2))

  def findMethodAst(meta: Meta, methodName: String): Option[AST] =
    findSMethodAST(meta, methodName).map(_._2)
