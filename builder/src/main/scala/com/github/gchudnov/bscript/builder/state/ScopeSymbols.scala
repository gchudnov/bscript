package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.builder.util.Ptr

case class ScopeSymbols(
  scopeSymbols: Map[Scope, List[Symbol]],
  symbolScopes: Map[Ptr[Symbol], Scope]
):

  def addScope(scope: Scope): ScopeSymbols =
    this.copy(scopeSymbols = this.scopeSymbols + (scope -> List.empty[Symbol]))

  def link(scope: Scope, symbol: Symbol): ScopeSymbols =
    // this.copy(scopeSymbols = this.scopeSymbols)
    ???

  def resolve(symbolName: String, start: Scope): Option[Symbol] =
    ???

object ScopeSymbols:
  val empty: ScopeSymbols =
    ScopeSymbols(
      scopeSymbols = Map.empty[Scope, List[Symbol]],
      symbolScopes = Map.empty[Ptr[Symbol], Scope]
    )

//   /**
//    * Adds a Symbol to the provided Scope
//    */
//   private def addScopeSymbol(symbol: Symbol, scope: Scope): ScopeSymbols =
//     val ss = scopeSymbols.getOrElse(Ptr(scope), List.empty[Symbol])

//     assert(!ss.contains(symbol), s"Symbol ${symbol.name} already exists in the collection of Scope Symbols ${scopeSymbols}.")

//     // TODO: check the TODO-list, there is a point to prevent redefinition of vars / methods; with the assert ^^ it is prohibited

//     // NOTE: if we're trying to insert the same symbol, replace it | NOT needed anymore
//     //    val ss1 = ss.indexOf(symbol) match
//     //      case -1 =>
//     //        ss :+ symbol
//     //      case n =>
//     //        ss.updated(n, symbol)

//     val newScopeSymbols = scopeSymbols + (Ptr(scope)  -> (ss :+ symbol))
//     val newSymbolScopes = symbolScopes + (Ptr(symbol) -> scope)

//     ScopeSymbols(newScopeSymbols, newSymbolScopes)




//   /**
//    * Resolve a symbol in the scope recursively up to the root
//    */
//   def resolve(name: String, in: Scope): Either[ScopeStateException, Symbol] =
//     maybeResolve(name, in)
//       .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' starting from Scope '${in.name}'"))

//   private def maybeResolve(name: String, in: Scope): Option[Symbol] =
//     scopeSymbols
//       .get(Ptr(in))
//       .flatMap(_.find(_.name == name))
//       .orElse(scopeTree.parentOf(in).flatMap(parentScope => maybeResolve(name, parentScope)))

//   /**
//    * Resolves a member of a scope by Name
//    */
//   def resolveMember(name: String, in: Scope): Either[ScopeStateException, Symbol] =
//     maybeResolveMember(name, in)
//       .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' in Scope '${in.name}'"))

//   private def maybeResolveMember(name: String, in: Scope): Option[Symbol] =
//     scopeSymbols
//       .get(Ptr(in))
//       .flatMap(_.find(_.name == name))

