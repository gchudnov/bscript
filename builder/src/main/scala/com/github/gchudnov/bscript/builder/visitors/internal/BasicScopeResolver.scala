package com.github.gchudnov.bscript.builder.visitors.internal

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.VarTypes
import com.github.gchudnov.bscript.builder.visitors.ScopeResolver
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.util.Casting

final class BasicScopeResolver(forest: Forest[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts, varTypes: VarTypes) extends ScopeResolver:
  import Casting.*

  override def resolveFldDecl(name: String, fType: Type, ast: AST): ScopeResolver =
    resolveVarDecl(name, fType, ast)

  override def resolveArgDecl(name: String, aType: Type, ast: AST): ScopeResolver = 
    resolveVarDecl(name, aType, ast)

  override def resolveVarDecl(name: String, vType: Type, ast: AST): ScopeResolver =
    val (sVar, sType) = (for
      scope        <- scopeFor(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))
      resolvedName <- resolveIn(name, scope).toRight(new BuilderException(s"Variable '${name}' cannot be resolved in scope '${scope}'"))
      resolvedType <- resolveUp(vType.name, scope).toRight(throw new BuilderException(s"Cannot resolve the variable type '${vType}' in scope '${scope}'"))
      sVar         <- resolvedName.asSVar
      sType        <- resolvedType.asType
    yield (sVar, sType)).toTry.get

    new BasicScopeResolver(forest, scopeSymbols, scopeAsts, varTypes.decl(sVar, sType))

  /**
   * Get scope for the given AST
   *
   * @param ast
   * @return
   */
  private[visitors] def scopeFor(ast: AST): Option[Scope] =
    scopeAsts.scope(ast)

  /**
   * Resolve the reference to a symbol, going up the scope hierarchy.
   *
   * @param sym
   *   symbol reference
   * @return
   *   resolved symbol
   */
  private[visitors] def resolveUp(name: String, start: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(start)
      .find(_.name == name)
      .orElse(forest.parentOf(start).flatMap(parent => resolveUp(name, parent)))

  /**
   * Resolve the reference to the symbol in the given scope only.
   *
   * @param sym
   *   symbol reference
   * @return
   *   resolved symbol
   */
  private[visitors] def resolveIn(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(in)
      .find(_.name == name)

  override def result: Meta =
    Meta(
      forest = forest,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts
    )
