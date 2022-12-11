package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.{ Named, SBlock, SBuiltInType, SMethod, SStruct, SVar, Scope, ScopeStateException, Symbol, Type, TypeRef }
import com.github.gchudnov.bscript.lang.util.{ Show, Transform }
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.lang.types.Types
import com.github.gchudnov.bscript.lang.util.Show

import scala.collection.mutable.StringBuilder as MStringBuilder

/**
 * Metadata - Scope & Symbol State
 *
 * NOTE: we're not storing symbols in Block, Method, Struct explicitly, since it is causing huge AST-rewrites on updates.
 *
 * To avoid rewrites, it is possible to use mutable Block, Method, Struct, but we want to avoid it to enable better traceability of mutations.
 *
 * @param scopeTree
 *   Scope tree where a child Scope points to a parent Scope (child -> parent)
 * @param scopeSymbols
 *   All symbols that belong to a scope; { Scope -> List[Symbol] } NOTE: We use `List[Symbol]` and NOT a Set to maintain the order Symbols were added.
 * @param symbolScopes
 *   Scope a Symbol belongs to; { Symbol -> Scope }
 * @param methodArgs
 *   Arguments that belong to a method; { Method -> List[Var] } NOTE: We use `List[Var]` and NOT a Set to maintain the order Vars were added.
 * @param methodRetTypes
 *   Return Type, associated with a method; { Method -> Type }
 * @param methodAsts
 *   AST, a Symbol references; { Symbol -> AST }
 * @param astScopes
 *   Scope, AST refers to; { AST -> Scope } -- parent scope for AST
 * @param varTypes
 *   Type, assigned to a variable { Var -> Type }. A Type for the variable might be changed, e.g. `auto` -> `long`.
 */
final case class Meta(
  scopeTree: ScopeTree,
  scopeSymbols: Map[Ptr[Scope], List[Symbol]], // Pass #1
  symbolScopes: Map[Ptr[Symbol], Scope],       // Pass #1
  methodArgs: Map[Ptr[SMethod], List[SVar]],   // Pass #1
  methodRetTypes: Map[Ptr[SMethod], Type],     // Pass   #2
  methodAsts: Map[Ptr[SMethod], AST],          // Pass   #2
  astScopes: Map[Ptr[AST], Scope],             // Pass #1
  varTypes: Map[Ptr[SVar], Type],              // Pass   #2
  evalTypes: Map[Ptr[AST], Type],              //
  promoteToTypes: Map[AST, Type],              //
):

  /**
   * Resolve a symbol in the scope recursively up to the root
   */
  def resolve(name: String, in: Scope): Either[ScopeStateException, Symbol] =
    maybeResolve(name, in)
      .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' starting from Scope '${in.name}'"))

  private def maybeResolve(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .get(Ptr(in))
      .flatMap(_.find(_.name == name))
      .orElse(scopeTree.parent(in).flatMap(parentScope => maybeResolve(name, parentScope)))

  /**
   * Resolves a member of a scope by Name
   */
  def resolveMember(name: String, in: Scope): Either[ScopeStateException, Symbol] =
    maybeResolveMember(name, in)
      .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' in Scope '${in.name}'"))

  private def maybeResolveMember(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .get(Ptr(in))
      .flatMap(_.find(_.name == name))

  /**
   * Defines a root Scope, not connected to any parent Scope
   *
   * NOTE: it could be multiple Scopes, not connected to the parent, but the use-case for that is not clear.
   */
  def defineBlock(block: SBlock): Meta =
    this.copy(scopeTree = scopeTree.add(block))

  /**
   * Defines a Block in the given Scope
   *
   * NOTE: Block is not a symbol
   */
  def defineBlock(block: SBlock, in: Scope): Meta =
    this.copy(scopeTree = scopeTree.link(block, in))

  /**
   * Defines a Method in the given Scope
   *
   * NOTE: a Method is a symbol as well.
   */
  def defineMethod(method: SMethod, in: Scope): Meta =
    defineSymbolScopeInScope(method, in)

  /**
   * Defines a Struct in the given Scope
   *
   * NOTE: a Struct is a symbol as well.
   */
  def defineStruct(struct: SStruct, in: Scope): Meta =
    defineSymbolScopeInScope(struct, in)

  /**
   * Defines a BuiltInType
   *
   * NOTE: a BuiltInType might be defined *only* in Block-scope.
   */
  def defineBuiltInType(t: SBuiltInType, in: SBlock): Meta =
    assert(scopeTree.parent(in).isEmpty, "A Built-in Symbols should be defined in The root scope (without parent scope)")
    defineSymbolInScope(t, in)

  /**
   * Defines a field in the Struct
   */
  def defineStructField(ss: SStruct, field: SVar): Meta =
    defineSymbolInScope(field, ss)

  /**
   * Defines a method argument (used to have an ordered list of arguments)
   */
  def defineMethodArg(ms: SMethod, arg: SVar): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(arg, ms)
    val newMethodArgs                      = addMethodArg(ms, arg)
    this.copy(scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes, methodArgs = newMethodArgs)

  /**
   * Defines a method type
   */
  def defineMethodRetType(sm: SMethod, retType: Type): Meta =
    val newMethodRetTypes = addMethodRetType(sm, retType)
    this.copy(methodRetTypes = newMethodRetTypes)

  /**
   * Defines a method AST
   */
  def defineMethodAST(sm: SMethod, ast: AST): Meta =
    val newMethodAsts = addMethodAST(sm, ast)
    this.copy(methodAsts = newMethodAsts)

  /**
   * Defines a scope for an AST
   */
  def defineASTScope(ast: AST, scope: Scope): Meta =
    val newAstScopes = addASTScope(ast, scope)
    this.copy(astScopes = newAstScopes)

  /**
   * Redefine AST in { AST -> Scope } bindings
   */
  def redefineASTScope(oldAst: AST, newAst: AST): Meta =
    val newAstScopes = replaceASTScope(oldAst, newAst)
    this.copy(astScopes = newAstScopes)

  def defineVar(v: SVar, in: SBlock): Meta =
    defineSymbolInScope(v, in)

  /**
   * Defines a Variable Symbol with the given Type. If the variable is already exists, redefines it.
   */
  def defineVarType(v: SVar, t: Type): Meta =
    val newVarTypes = addVarType(t, v)
    this.copy(varTypes = newVarTypes)

  /**
   * Gets the identifier of the symbol
   */
  def id(symbol: Symbol): Either[ScopeStateException, String] =
    Right(Ptr(symbol).hashCode().toString)

  /**
   * Get a Scope for the given AST
   */
  def scopeFor(ast: AST): Either[ScopeStateException, Scope] =
    astScopes
      .get(Ptr(ast))
      .toRight(new ScopeStateException(s"Scope for AST '${ast}' is not found."))

  /**
   * Get a Scope for the given Symbol
   */
  def scopeFor(symbol: Symbol): Either[ScopeStateException, Scope] =
    symbolScopes
      .get(Ptr(symbol))
      .toRight(new ScopeStateException(s"Scope for Symbol '${symbol.name}' is not found."))

  /**
   * Get a Type for the given Variable Symbol (SVar)
   */
  def typeFor(v: SVar): Either[ScopeStateException, Type] =
    varTypes
      .get(Ptr(v))
      .toRight(new ScopeStateException(s"Type for SVar '${v.name}' is not found."))

  /**
   * Get a Type for the given Method Symbol (SMethod)
   */
  def retTypeFor(m: SMethod): Either[ScopeStateException, Type] =
    methodRetTypes
      .get(Ptr(m))
      .toRight(new ScopeStateException(s"Type for SMethod '${m.name}' is not found."))

  /**
   * Gets all symbols in the given scope
   */
  def symbolsFor(s: Scope): List[Symbol] =
    scopeSymbols.getOrElse(Ptr(s), List.empty[Symbol])

  /**
   * Get Method Arguments Symbols
   */
  def methodArgSVars(m: SMethod): Either[Throwable, List[SVar]] =
    for
      _ <- Either.cond(
             symbolScopes.contains(Ptr(m.asInstanceOf[Symbol])),
             (),
             new ScopeStateException(s"Cannot get argument symbols for SMethod: '${m.name}', method not found")
           )
      sVars = methodArgs.getOrElse(Ptr(m), List.empty[SVar])
    yield sVars

  /**
   * Get Method Arguments Types
   */
  def methodArgTypes(m: SMethod): Either[Throwable, List[Type]] =
    for
      sVars <- methodArgSVars(m)
      types <- Transform.sequence(sVars.map(sVar => varTypes.get(Ptr(sVar)).toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found")))).map(_.toList)
    yield types

  /**
   * Gets a Map { SVar -> Type} for the arguments of a Method
   */
  def methodArgTypeMap(m: SMethod): Either[Throwable, Map[SVar, Type]] =
    for
      sVars <- methodArgSVars(m)
      types <- Transform
                 .sequence(
                   sVars.map { sVar =>
                     varTypes
                       .get(Ptr(sVar))
                       .map(t => (sVar, t))
                       .toRight(new ScopeStateException(s"Type for SVar ${sVar.name} is not found"))
                   }
                 )
                 .map(_.toMap)
    yield types

  /**
   * Gets AST for the given method
   */
  def methodAst(m: SMethod): Either[Throwable, AST] =
    methodAsts.get(Ptr(m)).toRight(new ScopeStateException(s"AST for the SMethod ${m.name} is not found"))

  /**
   * Returns types for the fields of the given struct.
   */
  def structTypes(sStruct: SStruct): Either[Throwable, Map[String, Type]] =
    Transform
      .sequence(symbolsFor(sStruct).toList.map { case (fs) =>
        fs match
          case x: SVar =>
            typeFor(x).left
              .map(ex => new ScopeStateException(s"Cannot find the type of the struct's field '${sStruct.name}.${x.name}'", ex))
              .map(t => x.name -> t)
          case other =>
            Left(new ScopeStateException(s"Expected struct field '${sStruct.name}.${fs.name}' to be SVar, got: '${other}'"))
      })
      .map(_.toMap)

  /**
   * Defines a Symbol in the given Scope
   */
  private def defineSymbolInScope(symbol: Symbol, scope: Scope): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(symbol, scope)
    this.copy(scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes)

  private def defineSymbolScopeInScope(symbolWithScope: Symbol with Scope, scope: Scope): Meta =
    val (newScopeSymbols, newSymbolScopes) = addScopeSymbol(symbolWithScope, scope)
    this.copy(scopeTree = scopeTree.link(symbolWithScope, scope), scopeSymbols = newScopeSymbols, symbolScopes = newSymbolScopes)

  /**
   * Adds a Symbol to the provided Scope
   */
  private def addScopeSymbol(symbol: Symbol, scope: Scope): (Map[Ptr[Scope], List[Symbol]], Map[Ptr[Symbol], Scope]) =
    val ss = scopeSymbols.getOrElse(Ptr(scope), List.empty[Symbol])

    assert(!ss.contains(symbol), s"Symbol ${symbol.name} already exists in the collection of Scope Symbols ${scopeSymbols}.")

    // TODO: check the TODO-list, there is a point to prevent redefinition of vars / methods; with the assert ^^ it is prohibited

    // NOTE: if we're trying to insert the same symbol, replace it | NOT needed anymore
    //    val ss1 = ss.indexOf(symbol) match
    //      case -1 =>
    //        ss :+ symbol
    //      case n =>
    //        ss.updated(n, symbol)

    val newScopeSymbols = scopeSymbols + (Ptr(scope)  -> (ss :+ symbol))
    val newSymbolScopes = symbolScopes + (Ptr(symbol) -> scope)

    (newScopeSymbols, newSymbolScopes)

  /**
   * Add a Var to the provided Method
   */
  private def addMethodArg(sMethod: SMethod, arg: SVar) =
    val args = methodArgs.getOrElse(Ptr(sMethod), List.empty[SVar])

    assert(!args.contains(arg), s"Argument ${arg.name} already exists in the collection of Method Arguments ${methodArgs}.")

    val newMethodArgs = methodArgs + (Ptr(sMethod) -> (args :+ arg))
    newMethodArgs

  /**
   * Add a Type to the provided Method to specify the return type.
   */
  private def addMethodRetType(sMethod: SMethod, t: Type) =
    val newMethodRetType = methodRetTypes + (Ptr(sMethod) -> t)
    newMethodRetType

  /**
   * Binds an AST to a Method. If a binding already exists, it will be overwritten
   */
  private def addMethodAST(sMethod: SMethod, ast: AST) =
    val newSymbolAsts = methodAsts + (Ptr(sMethod) -> ast)
    newSymbolAsts

  /**
   * Binds a Scope to an AST
   */
  private def addASTScope(ast: AST, scope: Scope): Map[Ptr[AST], Scope] =
    val newAstScopes = astScopes + (Ptr(ast) -> scope)
    newAstScopes

  /**
   * Replace AST in { AST -> Scope } binding
   */
  private def replaceASTScope(oldAst: AST, newAst: AST): Map[Ptr[AST], Scope] =
    val maybeScope = astScopes.get(Ptr(oldAst))
    assert(maybeScope.isDefined, "Cannot replace AST in { AST -> Scope } binding. AST to replace is not found.")
    maybeScope.fold(astScopes) { scope =>
      val newAstScopes = astScopes - Ptr(oldAst) + (Ptr(newAst) -> scope)
      newAstScopes
    }

  /**
   * Bind Type to a variable
   */
  private def addVarType(t: Type, v: SVar): Map[Ptr[SVar], Type] =
    val newVarTypes = varTypes + (Ptr(v) -> t)
    newVarTypes

object Meta:

  val empty: Meta =
    Meta(
      scopeTree = ScopeTree.empty,
      scopeSymbols = Map.empty[Ptr[Scope], List[Symbol]],
      symbolScopes = Map.empty[Ptr[Symbol], Scope],
      methodArgs = Map.empty[Ptr[SMethod], List[SVar]],
      methodRetTypes = Map.empty[Ptr[SMethod], Type],
      methodAsts = Map.empty[Ptr[SMethod], AST],
      astScopes = Map.empty[Ptr[AST], Scope],
      varTypes = Map.empty[Ptr[SVar], Type],
      evalTypes = Map.empty[Ptr[AST], Type],
      promoteToTypes = Map.empty[AST, Type],
    )

  def init(types: Types): Meta =
    val g = SBlock("#global")
    val m = Meta.empty
      .defineBlock(g)
      .defineBuiltInType(types.autoType, g)
      .defineBuiltInType(types.nothingType, g)
      .defineBuiltInType(types.voidType, g)
      .defineBuiltInType(types.boolType, g)
      .defineBuiltInType(types.i32Type, g)
      .defineBuiltInType(types.i64Type, g)
      .defineBuiltInType(types.f32Type, g)
      .defineBuiltInType(types.f64Type, g)
      .defineBuiltInType(types.decType, g)
      .defineBuiltInType(types.strType, g)
      .defineBuiltInType(types.dateType, g)
      .defineBuiltInType(types.datetimeType, g)
    m

  given Show[Meta] with
    import ScopeTree.{ *, given }
    import com.github.gchudnov.bscript.lang.util.LineOps.*

    extension (a: Meta)
      def show: String =
        val sb = new MStringBuilder
        sb.append("{\n")

        val treeStr           = tabTail(1, a.scopeTree.show)
        val scopeSymbolsStr   = tabTail(1, scopeSymbolsToMapStr(1)(a.scopeSymbols))
        val symbolScopesStr   = tabTail(1, symbolScopesToMapStr(1)(a.symbolScopes))
        val methodArgsStr     = tabTail(1, methodArgsToMapStr(1)(a.methodArgs))
        val methodRetTypesStr = tabTail(1, methodRetTypesToMapStr(1)(a.methodRetTypes))
        val methodAstsStr     = tabTail(1, methodAstsToMapStr(1)(a.methodAsts))
        val varTypesStr       = tabTail(1, varTypesToMapStr(1)(a.varTypes))
        val methodsStr        = tabTail(1, methodsToMapStr(1))

        sb.append(tabLine(1, s""""scopeTree": ${treeStr},\n"""))
        sb.append(tabLine(1, s""""scopeSymbols": ${scopeSymbolsStr},\n"""))
        sb.append(tabLine(1, s""""symbolScopes": ${symbolScopesStr},\n"""))
        sb.append(tabLine(1, s""""methodArgs": ${methodArgsStr},\n"""))
        sb.append(tabLine(1, s""""methodRetTypes": ${methodRetTypesStr},\n"""))
        sb.append(tabLine(1, s""""methodAsts": ${methodAstsStr},\n"""))
        sb.append(tabLine(1, s""""varTypes": ${varTypesStr},\n"""))
        sb.append(tabLine(1, s""""methods": ${methodsStr}\n"""))

        sb.append("}")
        sb.toString()

      def showMethod(sMethod: SMethod): String =
        val m          = Ptr(sMethod)
        val retTypeStr = a.methodRetTypes.get(m).map(_.name).getOrElse("?")
        val args       = a.methodArgs.getOrElse(m, List.empty[SVar])
        val argsWithTypes = args.map { arg =>
          val retType = a.varTypes.getOrElse(Ptr(arg), TypeRef("?"))
          s"${arg.name}: ${retType.name}"
        }

        s"""fn ${m.value.name}(${argsWithTypes.mkString(", ")}) -> ${retTypeStr}"""

      private def scopeSymbolsToMapStr(depth: Int)(m: Map[Ptr[Scope], List[Symbol]]): String =
        namedTuplesToMapStr(depth)(m.toList.map { case (k, vs) =>
          (k.value.asInstanceOf[Named], vs.map(_.asInstanceOf[Named]))
        })

      private def methodArgsToMapStr(depth: Int)(m: Map[Ptr[SMethod], List[SVar]]): String =
        namedTuplesToMapStr(depth)(m.toList.map { case (k, vs) =>
          (k.value.asInstanceOf[Named], vs.map(_.asInstanceOf[Named]))
        })

      private def symbolScopesToMapStr(depth: Int)(m: Map[Ptr[Symbol], Scope]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def methodRetTypesToMapStr(depth: Int)(m: Map[Ptr[SMethod], Type]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def varTypesToMapStr(depth: Int)(m: Map[Ptr[SVar], Type]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (k.value.asInstanceOf[Named], v.asInstanceOf[Named])
        })

      private def methodAstsToMapStr(depth: Int)(m: Map[Ptr[SMethod], AST]): String =
        namedTuples1ToMapStr(depth)(m.toList.map { case (k, v) =>
          (
            k.value.asInstanceOf[Named],
            new Named:
              val name: String = "(omitted)"
          )
        })

      private def namedTuples1ToMapStr(depth: Int)(xs: List[(Named, Named)]): String =
        val sb = new MStringBuilder
        sb.append(s"{\n")

        val lines = xs
          .sortBy(_._1.name)
          .map { case (k, v) =>
            tabLine(depth, s"""${quote(k.name)}: ${quote(v.name)}""")
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"}")
        sb.toString()

      private def namedTuplesToMapStr(depth: Int)(xs: List[(Named, Iterable[Named])]): String =
        val sb = new MStringBuilder
        sb.append(s"{\n")

        val lines = xs
          .sortBy(_._1.name)
          .map { case (k, vs) =>
            tabLine(depth, s"""${quote(k.name)}: ${listStr(asList(vs.toList.sortBy(_.name)).map(quote))}""")
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"}")
        sb.toString()

      private def methodsToMapStr(depth: Int): String =
        val sb = new MStringBuilder
        sb.append(s"[\n")

        val ms = a.scopeTree.vertices
          .filter(_.value.isInstanceOf[SMethod])
          .map(it => it.value.asInstanceOf[SMethod])
          .toList

        val lines = ms
          .sortBy(_.name)
          .map { m =>
            val methodStr = showMethod(m)
            tabLine(depth, quote(methodStr))
          }

        val body = lines.mkString(",\n")
        sb.append(body + (if body.nonEmpty then "\n" else ""))

        sb.append(s"]")
        sb.toString()

      private def asList(ss: Iterable[Named]): List[String] =
        ss.map(_.name).toList

      private def listStr(xs: List[String]): String =
        xs.mkString("[", ",", "]")
