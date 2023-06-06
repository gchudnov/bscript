package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.state.ScopeRef
import com.github.gchudnov.bscript.builder.state.Tree
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.VarTypes
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.symbols.types.TypeRef
import com.github.gchudnov.bscript.lang.util.Casting
import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.builder.pass.scoperesolve.InState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.OutState
import com.github.gchudnov.bscript.builder.BuilderException

private[scoperesolve] final case class PassState(
  // forest: Forest[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts,
  // varTypes: VarTypes
):
  import Casting.*

  /**
   * Resolve variable declaration:
   *   - resolve the name
   *   - resolve the type
   *
   * @param name
   *   variable name to resolve IN the scope
   * @param vType
   *   variable type to resolve IN & UP the scope
   * @param ast
   *   AST to resolve in
   * @return
   *   state with resolved variable and type
   */
  def resolveVarDecl(name: String, vType: TypeAST, ast: AST): PassState =
    for {
      scope <- tryScopeFor(ast)
      resolvedName <- tryResolveIn(name, scope)
    } yield ()
    ???

    // TODO: finish the implementation ^^^

  // def resolveVarDecl(name: String, vType: TypeAST, ast: AST): PassState =
  //   // val (sVar, sType) = (for
  //   //   scope        <- scopeFor(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))
  //   //   resolvedName <- resolveIn(name, scope).toRight(new BuilderException(s"Variable '${name}' cannot be resolved in scope '${scope}'"))
  //   //   resolvedType <- resolveUp(vType.name, scope).toRight(throw new BuilderException(s"Cannot resolve the variable type '${vType}' in scope '${scope}'"))
  //   //   sVar         <- resolvedName.asSVar
  //   //   sType        <- resolvedType.asType
  //   // yield (sVar, sType)).toTry.get

  //   // new BasicScopeResolver(forest, scopeSymbols, scopeAsts, varTypes.decl(sVar, sType))
  //   ???

  /**
   * Get scope for the given AST
   *
   * @param ast
   * @return
   */
  private[scoperesolve] def scopeFor(ast: AST): Option[Scope] =
    scopeAsts.scope(ast)

  /**
   * Get scope for the given AST
   *
   * @param ast
   *   AST to resolve in
   * @return
   *   resolved scope
   */
  private[scoperesolve] def tryScopeFor(ast: AST): Either[Throwable, Scope] =
    scopeFor(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))

  /**
   * Resolve the reference to the symbol in the given scope only.
   *
   * @param sym
   *   symbol reference
   * @return
   *   resolved symbol
   */
  private[scoperesolve] def resolveIn(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(in)
      .find(_.name == name)

  /**
    * Resolve the reference to the symbol in the given scope only.
    *
    * @param name symbol reference
    * @param in scope to resolve in
    * @return resolved symbol
    */
  private[scoperesolve] def tryResolveIn(name: String, in: Scope): Either[Throwable, Symbol] =
    resolveIn(name, in).toRight(new BuilderException(s"Symbol '${name}' cannot be resolved in scope '${in}'"))

  // /**
  //  * Resolve the reference to a symbol, going up the scope hierarchy.
  //  *
  //  * @param sym
  //  *   symbol reference
  //  * @return
  //  *   resolved symbol
  //  */
  // private[scoperesolve] def resolveUp(name: String, start: Scope): Option[Symbol] =
  //   scopeSymbols
  //     .symbols(start)
  //     .find(_.name == name)
  //     .orElse(forest.parentOf(start).flatMap(parent => resolveUp(name, parent)))

object PassState:

  def from(s: InState): PassState =
    PassState(
      scopeSymbols = s.scopeSymbols,
      scopeAsts = s.scopeAsts,
    )

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
    )
