package com.github.gchudnov.bscript.builder

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.{ Named, SBlock, SMethod, SStruct, SVar, Symbol, Type, TypeRef, SymbolRef }
import com.github.gchudnov.bscript.lang.util.{ Show, Transform }
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.types.TypeName
import com.github.gchudnov.bscript.lang.util.Show
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.builder.state.ForestCursor
import com.github.gchudnov.bscript.lang.symbols.Symbol

import scala.collection.mutable.StringBuilder as MStringBuilder
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.ScopeAsts


// /**
//  * Metadata - Scope & Symbol State
//  *
//  * NOTE: we're not storing symbols in Block, Method, Struct explicitly, since it is causing huge AST-rewrites on updates.
//  *
//  * To avoid rewrites, it is possible to use mutable Block, Method, Struct, but we want to avoid it to enable better traceability of mutations.
//  *
//  * @param scopeTree
//  *   Scope tree where a child Scope points to a parent Scope (child -> parent)
//  * @param scopeSymbols
//  *   All symbols that belong to a scope; { Scope -> List[Symbol] } NOTE: We use `List[Symbol]` and NOT a Set to maintain the order Symbols were added.
//  * @param symbolScopes
//  *   Scope a Symbol belongs to; { Symbol -> Scope }
//  * @param methodArgs
//  *   Arguments that belong to a method; { Method -> List[Var] } NOTE: We use `List[Var]` and NOT a Set to maintain the order Vars were added.
//  * @param methodRetTypes
//  *   Return Type, associated with a method; { Method -> Type }
//  * @param methodAsts
//  *   AST, a Symbol references; { Symbol -> AST }
//  * @param astScopes
//  *   Scope, AST refers to; { AST -> Scope } -- parent scope for AST
//  * @param varTypes
//  *   Type, assigned to a variable { Var -> Type }. A Type for the variable might be changed, e.g. `auto` -> `long`.
//  */
// final case class Meta(
//   scopeTree: Forest[Scope],
//   scopeSymbols: Map[Ptr[Scope], List[Symbol]], // Pass #1
//   symbolScopes: Map[Ptr[Symbol], Scope],       // Pass #1
//   methodArgs: Map[Ptr[SMethod], List[SVar]],   // Pass #1
//   methodRetTypes: Map[Ptr[SMethod], Type],     // Pass   #2
//   methodAsts: Map[Ptr[SMethod], AST],          // Pass   #2
//   astScopes: Map[Ptr[AST], Scope],             // Pass #1
//   varTypes: Map[Ptr[SVar], Type],              // Pass   #2
//   evalTypes: Map[Ptr[AST], Type],              //
//   promoteToTypes: Map[Ptr[AST], Type],         //
//   astSymbols: Map[Ptr[AST], Symbol]            //
// )

final case class Meta(
  forest: Forest[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts
)

object Meta:

  val empty: Meta = 
    Meta(
      forest = Forest.empty[Scope],
      scopeSymbols = ScopeSymbols.empty,
      scopeAsts = ScopeAsts.empty
    )

  extension (m: Meta)
    def symbolsByName(name: String): List[Symbol] =
      m.scopeSymbols.symbolsByName(name)


  // def typeNameForVarInScope(meta: Meta)(varName: String, scopeName: String): Either[Throwable, String] =
  //   for
  //     scope <- meta.scopeTree.vertices.find(_.value.name == scopeName).map(_.value).toRight(new RuntimeException(s"Scope '${scopeName}' not found."))
  //     symbol <-
  //       meta.scopeSymbols.get(Ptr(scope)).flatMap(_.find(_.name == varName)).toRight(new RuntimeException(s"Symbol '${varName}' not found in Scope '${scopeName}'."))
  //     sVar  <- Either.cond(symbol.isInstanceOf[SVar], symbol.asInstanceOf[SVar], new RuntimeException(s"Symbol '${varName}' in scope '${scopeName}' is not an SVar."))
  //     sType <- meta.varTypes.get(Ptr(sVar)).toRight(new RuntimeException(s"Type is not found for SVar '${varName}'."))
  //   yield sType.name

  // def findSymbolScope(meta: Meta, sym: Symbol): Option[Scope] =
  //   meta.symbolScopes.find(_._1.value == sym).map(_._2)

  // def findSymbolScope(meta: Meta, symName: String): Option[Scope] =
  //   meta.symbolScopes.find(_._1.value.name == symName).map(_._2)

  // def findMember(meta: Meta, name: String, in: Scope): Option[Symbol] =
  //   meta.resolveMember(name, in).toOption

  // def findType(meta: Meta, v: SVar): Option[Type] =
  //   meta.typeFor(v).toOption

  // def findRetType(meta: Meta, m: SMethod): Option[Type] =
  //   meta.retTypeFor(m).toOption

  // def findSymbolScopes(meta: Meta, symName: String): List[Scope] =
  //   meta.symbolScopes.filter(_._1.value.name == symName).values.toList

  // def findSMethodAST(meta: Meta, methodName: String): Option[(SMethod, AST)] =
  //   meta.methodAsts.find(_._1.value.name == methodName).map(it => (it._1.value, it._2))

  // def findMethodAst(meta: Meta, methodName: String): Option[AST] =
  //   findSMethodAST(meta, methodName).map(_._2)


  
  // type ScopeTree = Forest[Scope]

  // val empty: Meta =
  //   Meta(
  //     scopeTree = Forest.empty[Scope],
  //     scopeSymbols = Map.empty[Ptr[Scope], List[Symbol]],
  //     symbolScopes = Map.empty[Ptr[Symbol], Scope],
  //     methodArgs = Map.empty[Ptr[SMethod], List[SVar]],
  //     methodRetTypes = Map.empty[Ptr[SMethod], Type],
  //     methodAsts = Map.empty[Ptr[SMethod], AST],
  //     astScopes = Map.empty[Ptr[AST], Scope],
  //     varTypes = Map.empty[Ptr[SVar], Type],
  //     evalTypes = Map.empty[Ptr[AST], Type],
  //     promoteToTypes = Map.empty[Ptr[AST], Type],
  //     astSymbols = Map.empty[Ptr[AST], Symbol]
  //   )

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

//   /**
//    * Defines a root Scope, not connected to any parent Scope
//    *
//    * NOTE: it could be multiple Scopes, not connected to the parent, but the use-case for that is not clear.
//    */
//   def defineBlock(block: Scope): Meta =
//     this.copy(scopeTree = scopeTree.add(block))

//   /**
//    * Defines a Block in the given Scope
//    *
//    * NOTE: Block is not a symbol
//    */
//   def defineBlock(block: Scope, in: Scope): Meta =
//     this.copy(scopeTree = scopeTree.link(block, in))

//   /**
//    * Defines a Method in the given Scope
//    *
//    * NOTE: a Method is a symbol as well.
//    */
//   def defineMethod(method: SMethod, in: Scope): Meta =
//     defineSymbolScopeInScope(method, in)

//   /**
//    * Defines a Struct in the given Scope
//    *
//    * NOTE: a Struct is a symbol as well.
//    */
//   def defineStruct(struct: SStruct, in: Scope): Meta =
//     defineSymbolScopeInScope(struct, in)

//   /**
//    * Defines a BuiltInType
//    *
//    * NOTE: a BuiltInType might be defined *only* in Block-scope.
//    */
//   def defineBuiltInType(sym: Symbol, in: SBlock): Meta =
//     assert(scopeTree.parentOf(in).isEmpty, "A Built-in Symbols should be defined in The root scope (without parent scope)")
//     defineSymbolInScope(sym, in)

//   /**
//    * Defines a field in the Struct
//    */
//   def defineStructField(ss: SStruct, field: SVar): Meta =
//     defineSymbolInScope(field, ss)

//   /**
//    * Defines a method argument (used to have an ordered list of arguments)
//    */
//   def defineMethodArg(ms: SMethod, arg: SVar): Meta =
//     val scopeSymbols = addScopeSymbol(arg, ms)
//     val newMethodArgs                      = addMethodArg(ms, arg)
//     this.copy(
//       scopeSymbols = scopeSymbols.scopeSymbolsMap, 
//       symbolScopes = scopeSymbols.symbolScopeMap, 
//       methodArgs = newMethodArgs
//       )

//   /**
//    * Defines a method type
//    */
//   def defineMethodRetType(sm: SMethod, retType: Type): Meta =
//     val newMethodRetTypes = addMethodRetType(sm, retType)
//     this.copy(methodRetTypes = newMethodRetTypes)

//   /**
//    * Defines a method AST
//    */
//   def defineMethodAST(sm: SMethod, ast: AST): Meta =
//     val newMethodAsts = addMethodAST(sm, ast)
//     this.copy(methodAsts = newMethodAsts)

//   /**
//    * Defines a scope for an AST
//    */
//   def defineASTScope(ast: AST, scope: Scope): Meta =
//     val newAstScopes = addASTScope(ast, scope)
//     this.copy(astScopes = newAstScopes)

//   /**
//    * Redefine AST in { AST -> Scope } bindings
//    */
//   def redefineASTScope(oldAst: AST, newAst: AST): Meta =
//     val newAstScopes = replaceASTScope(oldAst, newAst)
//     this.copy(astScopes = newAstScopes)

//   def defineVar(v: SVar, in: SBlock): Meta =
//     defineSymbolInScope(v, in)

//   /**
//    * Add Eval Type to AST
//    */
//   def withEvalType(n: AST, t: Type): Meta =
//     val newEvalTypes =  addEvalType(n, t)
//     this.copy(evalTypes = newEvalTypes)

//   def withPromoteToType(n: AST, t: Type): Meta =
//     val newPromoteToTypes =  addPromoteToType(n, t)
//     this.copy(evalTypes = newPromoteToTypes)    

//   def withPromoteToType(n: AST, t: Option[Type]): Meta =
//     t.fold(this)(t => withPromoteToType(n, t))

//   /**
//     * Add Symbol to AST
//     */
//   def withASTSymbol(n: AST, s: Symbol): Meta = 
//     val newAstSymbols = addSymbol(n, s)
//     this.copy(astSymbols = newAstSymbols)

//   def ensureNoAST(n: AST): Meta =
//     require(!astSymbols.contains(Ptr(n)), s"astSymbols should not contain AST: ${n}")
//     require(!promoteToTypes.contains(Ptr(n)), s"promoteToTypes should not contain AST: ${n}")
//     require(!evalTypes.contains(Ptr(n)), s"evalTypes should not contain AST: ${n}")
//     require(!astScopes.contains(Ptr(n)), s"astScopes should not contain AST: ${n}")
//     require(!methodAsts.values.toList.contains(n), s"methodAsts should not contain AST: ${n}")
//     this

//   /**
//    * Defines a Variable Symbol with the given Type. If the variable is already exists, redefines it.
//    */
//   def defineVarType(v: SVar, t: Type): Meta =
//     val newVarTypes = addVarType(t, v)
//     this.copy(varTypes = newVarTypes)

//   /**
//    * Gets the identifier of the symbol
//    */
//   def id(symbol: Symbol): Either[ScopeStateException, String] =
//     Right(Ptr(symbol).hashCode().toString)

//   /**
//    * Get a Scope for the given AST
//    */
//   def scopeFor(ast: AST): Either[ScopeStateException, Scope] =
//     astScopes
//       .get(Ptr(ast))
//       .toRight(new ScopeStateException(s"Scope for AST '${ast}' is not found."))

//   /**
//    * Get a Scope for the given Symbol
//    */
//   def scopeFor(symbol: Symbol): Either[ScopeStateException, Scope] =
//     symbolScopes
//       .get(Ptr(symbol))
//       .toRight(new ScopeStateException(s"Scope for Symbol '${symbol.name}' is not found."))

//   /**
//    * Get a Type for the given Variable Symbol (SVar)
//    */
//   def typeFor(v: SVar): Either[ScopeStateException, Type] =
//     varTypes
//       .get(Ptr(v))
//       .toRight(new ScopeStateException(s"Type for SVar '${v.name}' is not found."))

//   /**
//    * Get a Type for the given Method Symbol (SMethod)
//    */
//   def retTypeFor(m: SMethod): Either[ScopeStateException, Type] =
//     methodRetTypes
//       .get(Ptr(m))
//       .toRight(new ScopeStateException(s"Type for SMethod '${m.name}' is not found."))

//   /**
//    * Gets all symbols in the given scope
//    */
//   def symbolsFor(s: Scope): List[Symbol] =
//     scopeSymbols.getOrElse(Ptr(s), List.empty[Symbol])

//   def evalTypeFor(n: AST): Either[ScopeStateException, Type] =
//     evalTypes
//       .get(Ptr(n))
//       .toRight(new ScopeStateException(s"EvalType for AST '${n}' is not found."))

//   /**
//    * Get Method Arguments Symbols
//    */
//   def methodArgSVars(m: SMethod): Either[Throwable, List[SVar]] =
//     for
//       _ <- Either.cond(
//              symbolScopes.contains(Ptr(m.asInstanceOf[Symbol])),
//              (),
//              new ScopeStateException(s"Cannot get argument symbols for SMethod: '${m.name}', method not found")
//            )
//       sVars = methodArgs.getOrElse(Ptr(m), List.empty[SVar])
//     yield sVars

//   /**
//    * Get Method Arguments Types
//    */
//   def methodArgTypes(m: SMethod): Either[Throwable, List[Type]] =
//     for
//       sVars <- methodArgSVars(m)
//       types <- Transform.sequence(sVars.map(sVar => varTypes.get(Ptr(sVar)).toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found")))).map(_.toList)
//     yield types

//   /**
//    * Gets a Map { SVar -> Type} for the arguments of a Method
//    */
//   def methodArgTypeMap(m: SMethod): Either[Throwable, Map[SVar, Type]] =
//     for
//       sVars <- methodArgSVars(m)
//       types <- Transform
//                  .sequence(
//                    sVars.map { sVar =>
//                      varTypes
//                        .get(Ptr(sVar))
//                        .map(t => (sVar, t))
//                        .toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found"))
//                    }
//                  )
//                  .map(_.toMap)
//     yield types

//   /**
//    * Gets AST for the given method
//    */
//   def methodAst(m: SMethod): Either[Throwable, AST] =
//     methodAsts.get(Ptr(m)).toRight(new ScopeStateException(s"AST for the SMethod ${m.name} is not found"))

//   /**
//    * Returns types for the fields of the given struct.
//    */
//   def structTypes(sStruct: SStruct): Either[Throwable, Map[String, Type]] =
//     Transform
//       .sequence(symbolsFor(sStruct).toList.map { case (fs) =>
//         fs match
//           case x: SVar =>
//             typeFor(x).left
//               .map(ex => new ScopeStateException(s"Cannot find the type of the struct's field '${sStruct.name}.${x.name}'", ex))
//               .map(t => x.name -> t)
//           case other =>
//             Left(new ScopeStateException(s"Expected struct field '${sStruct.name}.${fs.name}' to be SVar, got: '${other}'"))
//       })
//       .map(_.toMap)

//   /**
//    * Defines a Symbol in the given Scope
//    */
//   private def defineSymbolInScope(symbol: Symbol, scope: Scope): Meta =
//     val scopeSymbols = addScopeSymbol(symbol, scope)
//     this.copy(
//       scopeSymbols = scopeSymbols.scopeSymbolsMap, 
//       symbolScopes = scopeSymbols.symbolScopeMap
//       )

//   private def defineSymbolScopeInScope(symbolWithScope: Symbol with Scope, scope: Scope): Meta =
//     val scopeSymbols = addScopeSymbol(symbolWithScope, scope)
//     this.copy(
//       scopeTree = scopeTree.link(symbolWithScope, scope), 
//       scopeSymbols = scopeSymbols.scopeSymbolsMap, 
//       symbolScopes = scopeSymbols.symbolScopeMap
//       )

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
//    * Add a Var to the provided Method
//    */
//   private def addMethodArg(sMethod: SMethod, arg: SVar) =
//     val args = methodArgs.getOrElse(Ptr(sMethod), List.empty[SVar])

//     assert(!args.contains(arg), s"Argument ${arg.name} already exists in the collection of Method Arguments ${methodArgs}.")

//     val newMethodArgs = methodArgs + (Ptr(sMethod) -> (args :+ arg))
//     newMethodArgs

//   /**
//    * Add a Type to the provided Method to specify the return type.
//    */
//   private def addMethodRetType(sMethod: SMethod, t: Type) =
//     val newMethodRetType = methodRetTypes + (Ptr(sMethod) -> t)
//     newMethodRetType

//   /**
//    * Binds an AST to a Method. If a binding already exists, it will be overwritten
//    */
//   private def addMethodAST(sMethod: SMethod, ast: AST) =
//     val newSymbolAsts = methodAsts + (Ptr(sMethod) -> ast)
//     newSymbolAsts

//   /**
//    * Binds a Scope to an AST
//    */
//   private def addASTScope(ast: AST, scope: Scope): Map[Ptr[AST], Scope] =
//     val newAstScopes = astScopes + (Ptr(ast) -> scope)
//     newAstScopes

//   /**
//    * Replace AST in { AST -> Scope } binding
//    */
//   private def replaceASTScope(oldAst: AST, newAst: AST): Map[Ptr[AST], Scope] =
//     val maybeScope = astScopes.get(Ptr(oldAst))
//     assert(maybeScope.isDefined, "Cannot replace AST in { AST -> Scope } binding. AST to replace is not found.")
//     maybeScope.fold(astScopes) { scope =>
//       val newAstScopes = astScopes - Ptr(oldAst) + (Ptr(newAst) -> scope)
//       newAstScopes
//     }

//   /**
//    * Bind Type to a variable
//    */
//   private def addVarType(t: Type, v: SVar): Map[Ptr[SVar], Type] =
//     val newVarTypes = varTypes + (Ptr(v) -> t)
//     newVarTypes

//   private def addEvalType(n: AST, t: Type): Map[Ptr[AST], Type] =
//     val newEvalTypes = evalTypes + (Ptr(n) -> t)
//     newEvalTypes

//   private def addPromoteToType(n: AST, t: Type): Map[Ptr[AST], Type] =
//     val newPromoteToTypes = promoteToTypes + (Ptr(n) -> t)
//     newPromoteToTypes

//   private def addSymbol(n: AST, s: Symbol): Map[Ptr[AST], Symbol] =
//     val newAstSymbols = astSymbols + (Ptr(n) -> s)
//     newAstSymbols

// object Meta:

//   type ScopeSymbolsMap = Map[Ptr[Scope], List[Symbol]]
//   type SymbolScopeMap = Map[Ptr[Symbol], Scope]

//   final case class ScopeSymbols(
//     scopeSymbolsMap: ScopeSymbolsMap,
//     symbolScopeMap: SymbolScopeMap
//   )



//   def init(): Meta =
//     val g = SBlock("#global")
//     Meta.empty
//       .defineBlock(g)
//       .defineBuiltInType(SymbolRef.auto, g)
//       .defineBuiltInType(SymbolRef.nothing, g)
//       .defineBuiltInType(SymbolRef.void, g)
//       .defineBuiltInType(SymbolRef.bool, g)
//       .defineBuiltInType(SymbolRef.i32, g)
//       .defineBuiltInType(SymbolRef.i64, g)
//       .defineBuiltInType(SymbolRef.f32, g)
//       .defineBuiltInType(SymbolRef.f64, g)
//       .defineBuiltInType(SymbolRef.dec, g)
//       .defineBuiltInType(SymbolRef.str, g)
//       .defineBuiltInType(SymbolRef.date, g)
//       .defineBuiltInType(SymbolRef.datetime, g)
